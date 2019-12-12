/* Interface for NSDate for GNUStep
   Copyright (C) 1994, 1996, 1999 Free Software Foundation, Inc.

   This file is part of the GNUstep Base Library.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02111 USA.
  */

#ifndef __NSDate_h_GNUSTEP_BASE_INCLUDE
#define __NSDate_h_GNUSTEP_BASE_INCLUDE
#import	"../GNUstepBase/GSVersionMacros.h"

#import	"NSObjCRuntime.h"

#if	defined(__cplusplus)
extern "C" {
#endif

GS_EXPORT NSString * const NSSystemClockDidChangeNotification;

/**
 * Time interval difference between two dates, in seconds.
 */
typedef double NSTimeInterval;

/**
 * Time interval between the unix standard reference date of 1 January 1970
 * and the OpenStep reference date of 1 January 2001<br />
 * This number comes from:<br />
 * (((31 years * 365 days) + 8 days for leap years) = total number of days<br />
 * 24 hours * 60 minutes * 60 seconds)<br />
 * This ignores leap-seconds.
 */
GS_EXPORT const NSTimeInterval NSTimeIntervalSince1970;

#import	"NSObject.h"

@class NSArray;
@class NSCalendarDate;
@class NSData;
@class NSDictionary;
@class NSString;
@class NSTimeZone;
@class NSTimeZoneDetail;

@interface NSDate : NSObject <NSCoding,NSCopying>
{
}

/** Returns an autoreleased instance with the current date/time.
 */
+ (id) date;

#if OS_API_VERSION(GS_API_MACOSX, GS_API_LATEST)
/** Returns an autoreleased instance representing the date and time given
 * by string. The value of string may be a 'natural' specification as
 * specified by the preferences in the user defaults database, allowing
 * phrases like 'last tuesday'
 */
+ (id) dateWithNaturalLanguageString: (NSString*)string;

/**
 * <p>Returns an autoreleased instance representing the date and time given
 * by string. The value of string may be a 'natural' specification as
 * specified by the preferences in the user defaults database, allowing
 * phrases like 'last tuesday'
 * </p>
 * The locale contains keys such as -
 * <deflist>
 *   <term>NSDateTimeOrdering</term>
 *   <desc>Controls the use of ambiguous numbers. This is done as a
 *   sequence of the letters D(ay), M(onth), Y(ear), and H(our).
 *   YMDH means that the first number encountered is assumed to be a
 *   year, the second a month, the third a day, and the last an hour.
 *   </desc>
 *   <term>NSEarlierTimeDesignations</term>
 *   <desc>An array of strings for times in the past.<br />
 *   Defaults are <em>ago</em>, <em>last</em>, <em>past</em>, <em>prior</em>
 *   </desc>
 *   <term>NSHourNameDesignations</term>
 *   <desc>An array of arrays of strings identifying the time of day.
 *   Each array has an hour as its first value, and one or more words
 *   as subsequent values.<br />
 *   Defaults are: (0, midnight), (10, morning), (12, noon, lunch),
 *   (14, afternoon), (19, dinner).
 *   </desc>
 *   <term>NSLaterTimeDesignations</term>
 *   <desc>An array of strings for times in the future.<br />
 *   Default is <em>next</em>
 *   </desc>
 *   <term>NSNextDayDesignations</term>
 *   <desc>The day after today. Default is <em>tomorrow.</em>
 *   </desc>
 *   <term>NSNextNextDayDesignations</term>
 *   <desc>The day after tomorrow. Default is <em>nextday.</em>
 *   </desc>
 *   <term>NSPriorDayDesignations</term>
 *   <desc>The day before today. Default is <em>yesterday.</em>
 *   </desc>
 *   <term>NSThisDayDesignations</term>
 *   <desc>Identifies the current day. Default is <em>today.</em>
 *   </desc>
 *   <term>NSYearMonthWeekDesignations</term>
 *   <desc>An array giving the word for year, month, and week.<br />
 *   Defaults are <em>year</em>, <em>month</em> and <em>week</em>.
 *   </desc>
 * </deflist>
 */
+ (id) dateWithNaturalLanguageString: (NSString*)string
                              locale: (NSDictionary*)locale;
#endif

/** Returns an autoreleased instance with the date and time value given
 * by the string using the ISO standard format YYYY-MM-DD HH:MM:SS +/-HHHMM
 * (all the fields of which must be present).
 */
+ (id) dateWithString: (NSString*)description;

#if OS_API_VERSION(MAC_OS_X_VERSION_10_6,GS_API_LATEST)
/** Returns an autoreleased NSDate instance whose value is offset from
 * that of the given date by the specified interval in seconds.
 */
+ (id) dateWithTimeInterval: (NSTimeInterval)seconds sinceDate: (NSDate*)date;
#endif

/** Returns an autoreleased instance with the offset from the unix system
 * reference date of 1 January 1970, GMT.
 */
+ (id) dateWithTimeIntervalSince1970: (NSTimeInterval)seconds;

/** Returns an autoreleased instance with the offset from the current
 * date/time given by seconds (which may be fractional).
 */
+ (id) dateWithTimeIntervalSinceNow: (NSTimeInterval)seconds;

/** Returns an autoreleased instance with the offset from the OpenStep
 * reference date of 1 January 2001, GMT.
 */
+ (id) dateWithTimeIntervalSinceReferenceDate: (NSTimeInterval)seconds;

/** Returns an autoreleased instance with the date/time set in the far
 * past.
 */
+ (id) distantPast;

/** Returns an autoreleased instance with the date/time set in the far
 * future.
 */
+ (id) distantFuture;

/** Returns the time interval between the reference date and the current
 * time.
 */
+ (NSTimeInterval) timeIntervalSinceReferenceDate;

/** Returns an autorelease date instance formed by adding the specified
 * time interval in seconds to the receiver's time interval.
 */
- (id) addTimeInterval: (NSTimeInterval)seconds;

/** Returns the time interval between the receivers value and the
 * OpenStep reference date of 1 Jan 2001 GMT.
 */
- (NSComparisonResult) compare: (NSDate*)otherDate;

#if OS_API_VERSION(MAC_OS_X_VERSION_10_6,GS_API_LATEST)
/** Returns an autoreleased NSDate instance whose value is offset from
 * that of the receiver by the specified interval.
 */
- (id) dateByAddingTimeInterval: (NSTimeInterval)ti;
#endif

/** Returns an autoreleased instance of the [NSCalendarDate] class whose
 * date/time value is the same as that of the receiver, and which uses
 * the formatString and timeZone specified.
 */
- (NSCalendarDate*) dateWithCalendarFormat: (NSString*)formatString
				  timeZone: (NSTimeZone*)timeZone;

/** Returns a string representation of the receiver formatted according
 * to the default format string, time zone, and locale.
 */
- (NSString*) description;

/** Returns a string representation of the receiver formatted according
 * to the specified format string, time zone, and locale.
 */
- (NSString*) descriptionWithCalendarFormat: (NSString*)format
				   timeZone: (NSTimeZone*)aTimeZone
				     locale: (NSDictionary*)l;

/** Returns a string representation of the receiver formatted according
 * to the default format string and time zone, but using the given locale.
 */
- (NSString*) descriptionWithLocale: (id)locale;

/** Returns the earlier of the receiver and otherDate.<br />
 * If the two represent identical date/time values, returns the receiver.
 */
- (NSDate*) earlierDate: (NSDate*)otherDate;

/** Returns an instance initialised with the current date/time.
 */
- (id) init;

/** Returns an instance with the date and time value given
 * by the string using the ISO standard format YYYY-MM-DD HH:MM:SS +/-HHHMM
 * (all the fields of which must be present).
 */
- (id) initWithString: (NSString*)description;

/** Returns an instance with the given offset from anotherDate.
 */
- (id) initWithTimeInterval: (NSTimeInterval)secsToBeAdded
		  sinceDate: (NSDate*)anotherDate;

#if OS_API_VERSION(GS_API_MACOSX, GS_API_LATEST)
/** Returns an instance with the offset from the unix system
 * reference date of 1 January 1970, GMT.
 */
- (id) initWithTimeIntervalSince1970: (NSTimeInterval)seconds;
#endif

/** Returns an instance with the offset from the current date/time.
 */
- (id) initWithTimeIntervalSinceNow: (NSTimeInterval)secsToBeAdded;

/** <init />
 * Returns an instance with the given offset from the OpenStep
 * reference date of 1 January 2001, GMT.
 */
- (id) initWithTimeIntervalSinceReferenceDate: (NSTimeInterval)secs;

/** Returns NO if other is not a date, otherwise returns the result of
 * calling the -isEqualtoDate: method.
 */
- (BOOL) isEqual: (id)other;

/**  Returns whether the receiver is exactly equal to other, to the limit
 *  of the NSTimeInterval precision.<br />
 *  This is the behavior of the current MacOS-X system, not that of the
 *  OpenStep specification (which counted two dates within a second of
 *  each other as being equal).<br />
 *  The old behavior meant that two dates equal to a third date were not
 *  necessarily equal to each other (confusing), and meant that there was
 *  no reasonable way to use a date as a dictionary key or store dates
 *  in a set.
 */
- (BOOL) isEqualToDate: (NSDate*)other;

/** Returns the earlier of the receiver and otherDate.<br />
 * If the two represent identical date/time values, returns the receiver.
 */
- (NSDate*) laterDate: (NSDate*)otherDate;

/** Returns the time interval between the receivers value and the
 * unix system reference date of 1 January 1970, GMT.
 */
- (NSTimeInterval) timeIntervalSince1970;

/** Returns the time interval between the receivers value and that of the
 * otherDate argument.  If otherDate is earlier than the receiver, the
 * returned value will be positive, if it is later it will be negative.<br />
 * For current (2011) OSX compatibility, this method returns NaN if otherDate
 * is nil ... do not write code depending on that behavior.
 */
- (NSTimeInterval) timeIntervalSinceDate: (NSDate*)otherDate;

/** Returns the time interval between the receivers value and the
 * current date/time.  If the receiver represents a date/time in
 * the past this will be negative, if it is in the future the
 * returned value will be positive.
 */
- (NSTimeInterval) timeIntervalSinceNow;

/** Returns the time interval between the receivers value and the
 * OpenStep reference date of 1 Jan 2001 GMT.
 */
- (NSTimeInterval) timeIntervalSinceReferenceDate;

@end

#if	defined(__cplusplus)
}
#endif

#endif  /* __NSDate_h_GNUSTEP_BASE_INCLUDE*/
