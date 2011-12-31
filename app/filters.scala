package filters

import _root_.pretty.please
import play.mvc.Http.Request
import org.apache.commons.lang.StringUtils
import java.util.Date
import java.text.SimpleDateFormat
import pretty.please._

/**
 * Search Filters
 *
 * @author Felipe Oliveira [@_felipera]
 */
case class SearchFilters(var ne: Option[Double], var sw: Option[Double], var nw: Option[Double], var se: Option[Double], var geohashes: Option[List[String]] = None, var zoom: Option[Int] = None) {

    /**
     * Log Debug
     */
    please log "NE: " + ne
    please log "SW: " + sw
    please log "NW: " + nw
    please log "SE: " + se
    please log "Geohashes: " + geohashes
    please log "Zoom: " + zoom

    /**
     * Format Date
     */
    def format(date: Date) = dateFormat format date

    /**
     * Geohash Precision
     */
    def geohashPrecision: Int = zoom match {
        case Some(z: Int) if z > 0 => 22 - z
        case _ => 1
    }

    /**
     * Geohash Suffix
     */
    def geohashSuffix: String = geohashPrecision toString

    /**
     * Has Bounds
     */
    def hasBounds: Boolean = {
        please log "Has Bounds? NE: " + ne + ", SW: " + sw + ", NW: " + nw + ", SE: " + se
        if (ne.valid && nw.valid && nw.valid && se.valid) {
            please log "Yes!"
            true
        } else {
            please log "No!"
            false
        }
    }

    /**
     * To Query String
     */
    def toQueryString = {
        // Start
        val queryString = new StringBuilder

        // Map Bounds
        if (hasBounds) {
            queryString append "ne=" append ne append "&"
            queryString append "sw=" append sw append "&"
            queryString append "nw=" append nw append "&"
            queryString append "se=" append se append "&"
        }

        // Zoom
        zoom match {
            case Some(s) => queryString append "zoom=" append s append "&"
            case _ => please log "No zoom level defined!"
        }

        // Log Debug
        please log "Query String: " + queryString
        queryString.toString
    }

}