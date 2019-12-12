/* CFLocale.h
   
   Copyright (C) 2010 Free Software Foundation, Inc.
   
   Written by: Stefan Bidigaray
   Date: March, 2011
   
   This file is part of the GNUstep CoreBase Library.
   
   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; see the file COPYING.LIB.
   If not, see <http://www.gnu.org/licenses/> or write to the 
   Free Software Foundation, 51 Franklin Street, Fifth Floor, 
   Boston, MA 02110-1301, USA.
*/

#ifndef __COREFOUNDATION_CFLOCALE__
#define __COREFOUNDATION_CFLOCALE__ 1

#include "CFBase.h"

#include "CFArray.h"
#include "CFDictionary.h"

#if OS_API_VERSION(MAC_OS_X_VERSION_10_3, GS_API_LATEST)

CF_EXTERN_C_BEGIN

/** \defgroup CFLocaleRef CFLocale Reference
    \brief CFLocale provides basic functionality for language and/or region
    specific operations.
    
    Locale-sensitive operations, such as collation, calendars and
    capitalization, may use CFLocale objects to provide language and/or region
    specific functionality.
  
    CFLocale is "toll-free bridged" to NSLocale.
    
    \{
 */
typedef const struct __CFLocale *CFLocaleRef;

/** 
 */
enum
{
  kCFLocaleLanguageDirectionUnknown     = 0,
  kCFLocaleLanguageDirectionLeftToRight = 1,
  kCFLocaleLanguageDirectionRightToLeft = 2,
  kCFLocaleLanguageDirectionTopToBottom = 3,
  kCFLocaleLanguageDirectionBottomToTop = 4
};
typedef CFIndex CFLocaleLanguageDirection;

/** \name CFLocale Property Keys
    \{
 */
CF_EXPORT const CFStringRef kCFLocaleMeasurementSystem; /* CFString */
CF_EXPORT const CFStringRef kCFLocaleDecimalSeparator; /* CFString */
CF_EXPORT const CFStringRef kCFLocaleGroupingSeparator; /* CFString */
CF_EXPORT const CFStringRef kCFLocaleCurrencySymbol; /* CFString */
CF_EXPORT const CFStringRef kCFLocaleCurrencyCode; /* CFString */
#if OS_API_VERSION(MAC_OS_X_VERSION_10_4, GS_API_LATEST)
CF_EXPORT const CFStringRef kCFLocaleIdentifier; /* CFString */
CF_EXPORT const CFStringRef kCFLocaleLanguageCode; /* CFString */
CF_EXPORT const CFStringRef kCFLocaleCountryCode; /* CFString */
CF_EXPORT const CFStringRef kCFLocaleScriptCode; /* CFString */
CF_EXPORT const CFStringRef kCFLocaleVariantCode; /* CFString */
CF_EXPORT const CFStringRef kCFLocaleExemplarCharacterSet; /* CFCharacterSet */
CF_EXPORT const CFStringRef kCFLocaleCalendarIdentifier; /* CFString */
CF_EXPORT const CFStringRef kCFLocaleCalendar; /* CFCalendar */
CF_EXPORT const CFStringRef kCFLocaleCollationIdentifier; /* CFString */
CF_EXPORT const CFStringRef kCFLocaleUsesMetricSystem; /* CFBoolean */
#endif
#if OS_API_VERSION(MAC_OS_X_VERSION_10_6, GS_API_LATEST)
CF_EXPORT const CFStringRef kCFLocaleCollatorIdentifier; /* CFString */
CF_EXPORT const CFStringRef kCFLocaleQuotationBeginDelimiterKey; /* CFString */
CF_EXPORT const CFStringRef kCFLocaleQuotationEndDelimiterKey; /* CFString */
CF_EXPORT const CFStringRef kCFLocaleAlternateQuotationBeginDelimiterKey; /* CFString */
CF_EXPORT const CFStringRef kCFLocaleAlternateQuotationEndDelimiterKey; /* CFString */
#endif
/** \} */

/** \name CFCalendar Identifiers
    \{
 */
CF_EXPORT const CFStringRef kCFGregorianCalendar;
#if OS_API_VERSION(MAC_OS_X_VERSION_10_4, GS_API_LATEST)
CF_EXPORT const CFStringRef kCFBuddhistCalendar;
CF_EXPORT const CFStringRef kCFChineseCalendar;
CF_EXPORT const CFStringRef kCFHebrewCalendar;
CF_EXPORT const CFStringRef kCFIslamicCalendar;
CF_EXPORT const CFStringRef kCFIslamicCivilCalendar;
CF_EXPORT const CFStringRef kCFJapaneseCalendar;
#endif
#if OS_API_VERSION(MAC_OS_X_VERSION_10_6, GS_API_LATEST)
CF_EXPORT const CFStringRef kCFRepublicOfChinaCalendar;
CF_EXPORT const CFStringRef kCFPersianCalendar;
CF_EXPORT const CFStringRef kCFIndianCalendar;
CF_EXPORT const CFStringRef kCFISO8601Calendar;
#endif
/** \} */

/** CFLocale Change Notification
 */
CF_EXPORT const CFStringRef kCFLocaleCurrentLocaleDidChangeNotification;



/** \name Creating a Locale
    \{
 */
CF_EXPORT CFLocaleRef
CFLocaleCopyCurrent (void);

CF_EXPORT CFLocaleRef
CFLocaleCreate (CFAllocatorRef allocator,
                CFStringRef localeIdent);

CF_EXPORT CFLocaleRef
CFLocaleCreateCopy (CFAllocatorRef allocator,
                    CFLocaleRef locale);

CF_EXPORT CFLocaleRef
CFLocaleGetSystem (void);
/** \} */

/** \name Getting System Locale Information
    \{
 */
#if OS_API_VERSION(MAC_OS_X_VERSION_10_4, GS_API_LATEST)
CF_EXPORT CFArrayRef
CFLocaleCopyAvailableLocaleIdentifiers (void);
#endif
/** \} */

/** \name Getting ISO Information
    \{
 */
#if OS_API_VERSION(MAC_OS_X_VERSION_10_4, GS_API_LATEST)
CF_EXPORT CFArrayRef
CFLocaleCopyISOCountryCodes (void);

CF_EXPORT CFArrayRef
CFLocaleCopyISOLanguageCodes (void);

CF_EXPORT CFArrayRef
CFLocaleCopyISOCurrencyCodes (void);
#endif

#if OS_API_VERSION(MAC_OS_X_VERSION_10_5, GS_API_LATEST)
CF_EXPORT CFArrayRef
CFLocaleCopyCommonISOCurrencyCodes (void);
#endif
/** \{ */

/** \name Accessing Language Information
    \{
 */
#if OS_API_VERSION(MAC_OS_X_VERSION_10_5, GS_API_LATEST)
CF_EXPORT CFArrayRef
CFLocaleCopyPreferredLanguages (void);
#endif

#if OS_API_VERSION(MAC_OS_X_VERSION_10_6, GS_API_LATEST)
CF_EXPORT CFLocaleLanguageDirection
CFLocaleGetLanguageCharacterDirection (CFStringRef isoLangCode);

CF_EXPORT CFLocaleLanguageDirection
CFLocaleGetLanguageLineDirection (CFStringRef isoLangCode);
#endif
/** \} */

/** \name Getting Information About a Locale
    \{
 */
CF_EXPORT CFStringRef
CFLocaleCopyDisplayNameForPropertyValue (CFLocaleRef displayLocale,
                                         CFStringRef key,
                                         CFStringRef value);

CF_EXPORT CFTypeRef
CFLocaleGetValue (CFLocaleRef locale,
                  CFStringRef key);

CF_EXPORT CFStringRef
CFLocaleGetIdentifier (CFLocaleRef locale);
/** \} */

/** \name Getting and Creating Locale Identifiers
    \{
 */
CF_EXPORT CFStringRef
CFLocaleCreateCanonicalLocaleIdentifierFromString (CFAllocatorRef allocator,
                                                   CFStringRef localeIdent);

#if OS_API_VERSION(MAC_OS_X_VERSION_10_4, GS_API_LATEST)
CF_EXPORT CFStringRef
CFLocaleCreateCanonicalLanguageIdentifierFromString (CFAllocatorRef allocator,
                                                     CFStringRef localeIdent);

CF_EXPORT CFDictionaryRef
CFLocaleCreateComponentsFromLocaleIdentifier (CFAllocatorRef allocator,
                                              CFStringRef localeIdent);

CF_EXPORT CFStringRef
CFLocaleCreateLocaleIdentifierFromComponents (CFAllocatorRef allocator,
                                              CFDictionaryRef dictionary);
#endif
/** \} */

/** \name Windows Locale Codes
    \{
 */
#if OS_API_VERSION(MAC_OS_X_VERSION_10_6, GS_API_LATEST)
CF_EXPORT CFStringRef
CFLocaleCreateLocaleIdentifierFromWindowsLocaleCode (CFAllocatorRef allocator,
                                                     UInt32 lcid);

CF_EXPORT UInt32
CFLocaleGetWindowsLocaleCodeFromLocaleIdentifier (CFStringRef localeIdent);
#endif
/** \} */

/** \name Getting the CFLocale Type ID
    \{
 */
CF_EXPORT CFTypeID
CFLocaleGetTypeID (void);
/** \} */

/** \} */

CF_EXTERN_C_END

#endif /* OS_API_VERSION >= MAC_OS_X_VERSION_10_3 */

#endif /* __COREFOUNDATION_CFLOCALE__ */

