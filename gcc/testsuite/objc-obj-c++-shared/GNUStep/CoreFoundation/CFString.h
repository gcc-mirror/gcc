/* CFString.h
   
   Copyright (C) 2010 Free Software Foundation, Inc.
   
   Written by: Stefan Bidigaray
   Date: January, 2010
   
   This file is part of CoreBase.
   
   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.         See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; see the file COPYING.LIB.
   If not, see <http://www.gnu.org/licenses/> or write to the 
   Free Software Foundation, 51 Franklin Street, Fifth Floor, 
   Boston, MA 02110-1301, USA.
*/ 

#ifndef __COREFOUNDATION_CFSTRING_H__
#define __COREFOUNDATION_CFSTRING_H__

#include "CFBase.h"
#include "CFArray.h"
#include "CFCharacterSet.h"
#include "CFData.h"
#include "CFDictionary.h"
#include "CFLocale.h"

#include <stdarg.h>

CF_EXTERN_C_BEGIN

/** \defgroup CFStringRef CFString Reference
    \brief The CFString type defines opaque objects representing strings.

    CFString is "toll-free bridged" to NSString.

    \{
 */

/*
 * Data Types
 */
typedef UInt32 CFStringEncoding;

/*
 * Constants
 */
typedef enum
{
  kCFCompareCaseInsensitive = 1,
  kCFCompareBackwards = 4,
  kCFCompareAnchored = 8,
  kCFCompareNonliteral = 16,
  kCFCompareLocalized = 32,
  kCFCompareNumerically = 64,
#if OS_API_VERSION(MAC_OS_X_VERSION_10_5, GS_API_LATEST)
  kCFCompareDiacriticInsensitive = 128,
  kCFCompareWidthInsensitive = 256,
  kCFCompareForcedOrdering = 512
#endif
} CFStringCompareFlags;

enum CFStringBuiltInEncodings
{
  kCFStringEncodingMacRoman = 0,
  kCFStringEncodingWindowsLatin1 = 0x0500,
  kCFStringEncodingISOLatin1 = 0x0201,
  kCFStringEncodingNextStepLatin = 0x0B01,
  kCFStringEncodingASCII = 0x0600,
  kCFStringEncodingUnicode = 0x0100,
  kCFStringEncodingUTF8 = 0x08000100,
  kCFStringEncodingNonLossyASCII = 0x0BFF,
#if OS_API_VERSION(MAC_OS_X_VERSION_10_4, GS_API_LATEST)
  kCFStringEncodingUTF16 = 0x0100,
  kCFStringEncodingUTF16BE = 0x10000100,
  kCFStringEncodingUTF16LE = 0x14000100,
  kCFStringEncodingUTF32 = 0x0c000100,
  kCFStringEncodingUTF32BE = 0x18000100,
  kCFStringEncodingUTF32LE = 0x1c000100
#endif
};

#if OS_API_VERSION(MAC_OS_X_VERSION_10_2, GS_API_LATEST)
# define kCFStringEncodingInvalidId (0xffffffffU)
#endif

/** \def CFSTR(x)
    \brief Creates a constant string object.
    
    \note This macro will create the constant string at runtime.
 */
/* The 'pure' attribute tells the compiler that this function will always
   return the same result with the same input.  If it has any skill, then
   constant propagation passes will magically make sure that this function is
   called as few times as possible. */
CF_EXPORT CFStringRef
__CFStringMakeConstantString (const char *str) GS_PURE_FUNCTION;
//#define CFSTR(x) __CFStringMakeConstantString("" x "")

#ifdef __CONSTANT_CFSTRINGS__
#define CFSTR(x)  ((CFStringRef) __builtin___CFStringMakeConstantString ("" x ""))
#else
#define CFSTR(x)  __CFStringMakeConstantString("" x "")
#endif

/** \name Creating a CFString
    \{
 */
CF_EXPORT CFArrayRef
CFStringCreateArrayBySeparatingStrings (CFAllocatorRef alloc,
  CFStringRef theString, CFStringRef separatorString);

CF_EXPORT CFStringRef
CFStringCreateByCombiningStrings (CFAllocatorRef alloc, CFArrayRef theArray,
  CFStringRef separatorString);

CF_EXPORT CFStringRef
CFStringCreateCopy (CFAllocatorRef alloc, CFStringRef theString);

CF_EXPORT CFStringRef
CFStringCreateFromExternalRepresentation (CFAllocatorRef alloc, CFDataRef data,
  CFStringEncoding encoding);

CF_EXPORT CFStringRef
CFStringCreateWithBytes (CFAllocatorRef alloc, const UInt8 *bytes,
  CFIndex numBytes, CFStringEncoding encoding, Boolean isExternalRepresentation);

CF_EXPORT CFStringRef
CFStringCreateWithCharacters (CFAllocatorRef alloc, const UniChar *chars,
  CFIndex numChars);

CF_EXPORT CFStringRef
CFStringCreateWithCharactersNoCopy (CFAllocatorRef alloc, const UniChar *chars,
  CFIndex numChars, CFAllocatorRef contentsDeallocator);

CF_EXPORT CFStringRef
CFStringCreateWithCString (CFAllocatorRef alloc, const char *cStr,
  CFStringEncoding encoding);

CF_EXPORT CFStringRef
CFStringCreateWithCStringNoCopy (CFAllocatorRef alloc, const char *cStr,
  CFStringEncoding encoding, CFAllocatorRef contentsDeallocator);

CF_EXPORT CFStringRef
CFStringCreateWithFormat (CFAllocatorRef alloc, CFDictionaryRef formatOptions,
  CFStringRef format, ...);

CF_EXPORT CFStringRef
CFStringCreateWithFormatAndArguments (CFAllocatorRef alloc,
  CFDictionaryRef formatOptions, CFStringRef format, va_list arguments);

CF_EXPORT CFStringRef
CFStringCreateWithSubstring (CFAllocatorRef alloc, CFStringRef str,
  CFRange range);

#if OS_API_VERSION(MAC_OS_X_VERSION_10_4, GS_API_LATEST)
CF_EXPORT CFStringRef
CFStringCreateWithFileSystemRepresentation (CFAllocatorRef alloc,
  const char *buffer);
#endif

#if OS_API_VERSION(MAC_OS_X_VERSION_10_5, GS_API_LATEST)
CF_EXPORT CFStringRef
CFStringCreateWithBytesNoCopy (CFAllocatorRef alloc, const UInt8 *bytes,
  CFIndex numBytes, CFStringEncoding encoding, Boolean isExternalReprentation,
  CFAllocatorRef contentsDeallocator);
#endif
/** \} */

/** \name Searching CFStrings
    \{
 */
CF_EXPORT CFArrayRef
CFStringCreateArrayWithFindResults (CFAllocatorRef alloc, CFStringRef theString,
  CFStringRef stringToFind, CFRange rangeToSearch,
  CFStringCompareFlags compareOptions);

CF_EXPORT CFRange
CFStringFind (CFStringRef theString, CFStringRef stringToFind,
  CFStringCompareFlags compareOptions);

CF_EXPORT Boolean
CFStringFindWithOptions (CFStringRef theString, CFStringRef stringToFind,
  CFRange rangeToSearch, CFStringCompareFlags searchOptions, CFRange *result);

CF_EXPORT Boolean
CFStringFindWithOptionsAndLocale (CFStringRef theString,CFStringRef stringToFind,
  CFRange rangeToSearch, CFStringCompareFlags searchOptions,
  CFLocaleRef locale, CFRange *result);

CF_EXPORT void
CFStringGetLineBounds (CFStringRef theString, CFRange range,
  CFIndex *lineBeginIndex, CFIndex *lineEndIndex, CFIndex *contentsEndIndex);

#if OS_API_VERSION(MAC_OS_X_VERSION_10_2, GS_API_LATEST)
CF_EXPORT Boolean
CFStringFindCharacterFromSet (CFStringRef theString, CFCharacterSetRef theSet,
  CFRange rangeToSearch, CFStringCompareFlags searchOptions, CFRange *result);
#endif

#if OS_API_VERSION(MAC_OS_X_VERSION_10_5, GS_API_LATEST)
CF_EXPORT void
CFStringGetParagraphBounds (CFStringRef string, CFRange range,
  CFIndex *parBeginIndex, CFIndex *parEndIndex, CFIndex *contentsEndIndex);
#endif
/** \} */

/** \name Comparing String
    \{
 */
CF_EXPORT CFComparisonResult
CFStringCompare (CFStringRef theString1, CFStringRef theString2,
  CFStringCompareFlags compareOptions);

CF_EXPORT CFComparisonResult
CFStringCompareWithOptions (CFStringRef theString1, CFStringRef theString2,
  CFRange rangeToCOmpare, CFStringCompareFlags compareOptions);

CF_EXPORT Boolean
CFStringHasPrefix (CFStringRef theString, CFStringRef prefix);

CF_EXPORT Boolean
CFStringHasSuffix (CFStringRef theString, CFStringRef suffix);

#if OS_API_VERSION(MAC_OS_X_VERSION_10_5, GS_API_LATEST)
CF_EXPORT CFComparisonResult
CFStringCompareWithOptionsAndLocale (CFStringRef theString1,
  CFStringRef theString2, CFRange rangeToCOmpare,
  CFStringCompareFlags compareOptions, CFLocaleRef locale);
#endif
/** \} */

/** \name Accessing Characters
    \{
 */
CF_EXPORT CFDataRef
CFStringCreateExternalRepresentation (CFAllocatorRef alloc,
  CFStringRef theString, CFStringEncoding encoding, UInt8 lossByte);

CF_EXPORT CFIndex
CFStringGetBytes (CFStringRef theString, CFRange range,
  CFStringEncoding encoding, UInt8 lossByte, Boolean isExternalRepresentation,
  UInt8 *buffer, CFIndex maxBufLen, CFIndex *usedBufLen);

CF_EXPORT UniChar
CFStringGetCharacterAtIndex (CFStringRef theString, CFIndex idx);

CF_EXPORT void
CFStringGetCharacters (CFStringRef theString, CFRange range, UniChar *buffer);

CF_EXPORT const UniChar *
CFStringGetCharactersPtr (CFStringRef theString);

CF_EXPORT Boolean
CFStringGetCString (CFStringRef theString, char *buffer, CFIndex bufferSize,
  CFStringEncoding encoding);

CF_EXPORT const char *
CFStringGetCStringPtr (CFStringRef theString, CFStringEncoding encoding);

CF_EXPORT CFIndex
CFStringGetLength (CFStringRef str);

CF_EXPORT CFRange
CFStringGetRangeOfComposedCharactersAtIndex (CFStringRef theString,
  CFIndex theIndex);

#if OS_API_VERSION(MAC_OS_X_VERSION_10_6, GS_API_LATEST)
CF_EXPORT UTF32Char
CFStringGetLongCharacterForSurrogatePair (UniChar surrogateHigh,
  UniChar surrogateLow);

CF_EXPORT Boolean
CFStringGetSurrogatePairForLongCharacter (UTF32Char character,
  UniChar *surrogates);

CF_EXPORT Boolean
CFStringIsSurrogateHighCharacter (UniChar character);

CF_EXPORT Boolean
CFStringIsSurrogateLowCharacter (UniChar character);
#endif
/** \} */

/** \name Working with Encodings
    \{
 */
CF_EXPORT CFStringRef
CFStringConvertEncodingToIANACharSetName (CFStringEncoding encoding);

CF_EXPORT unsigned long
CFStringConvertEncodingToNSStringEncoding (CFStringEncoding encoding);

UInt32
CFStringConvertEncodingToWindowsCodepage (CFStringEncoding encoding);

CF_EXPORT CFStringEncoding
CFStringConvertIANACharSetNameToEncoding (CFStringRef theString);

CF_EXPORT CFStringEncoding
CFStringConvertNSStringEncodingToEncoding (unsigned long encoding);

CF_EXPORT CFStringEncoding
CFStringConvertWindowsCodepageToEncoding (UInt32 codepage);

CF_EXPORT CFStringEncoding
CFStringGetFastestEncoding (CFStringRef theString);

CF_EXPORT const CFStringEncoding *
CFStringGetListOfAvailableEncodings (void);

CF_EXPORT CFIndex
CFStringGetMaximumSizeForEncoding (CFIndex length, CFStringEncoding encoding);

CF_EXPORT CFStringEncoding
CFStringGetMostCompatibleMacStringEncoding (CFStringEncoding encoding);

CF_EXPORT CFStringRef
CFStringGetNameOfEncoding (CFStringEncoding encoding);

CF_EXPORT CFStringEncoding
CFStringGetSmallestEncoding (CFStringRef theString);

CF_EXPORT CFStringEncoding
CFStringGetSystemEncoding (void);

CF_EXPORT Boolean
CFStringIsEncodingAvailable (CFStringEncoding encoding);

#if OS_API_VERSION(MAC_OS_X_VERSION_10_4, GS_API_LATEST)
CF_EXPORT Boolean
CFStringGetFileSystemRepresentation (CFStringRef string, char *buffer,
  CFIndex maxBufLen);

CF_EXPORT CFIndex
CFStringGetMaximumSizeOfFileSystemRepresentation (CFStringRef string);
#endif
/** \} */

/** \name Getting Numeric Values
    \{
 */
CF_EXPORT double
CFStringGetDoubleValue (CFStringRef str);

CF_EXPORT SInt32
CFStringGetIntValue (CFStringRef str);
/** \} */

/** \name Getting String Properties
    \{
 */
CF_EXPORT void
CFShow (CFTypeRef obj);

CF_EXPORT void
CFShowStr (CFStringRef str);

CF_EXPORT CFTypeID
CFStringGetTypeID (void);
/** \} */



/** \name Pascal Strings
    \{
 */
CF_EXPORT CFStringRef
CFStringCreateWithPascalString (CFAllocatorRef alloc, ConstStr255Param pStr,
  CFStringEncoding encoding);

CF_EXPORT CFStringRef
CFStringCreateWithPascalStringNoCopy (CFAllocatorRef alloc,
  ConstStr255Param pStr, CFStringEncoding encoding,
  CFAllocatorRef contentsDeallocate);

CF_EXPORT Boolean
CFStringGetPascalString (CFStringRef theString, StringPtr buffer,
  CFIndex bufferSize, CFStringEncoding encoding);

CF_EXPORT ConstStringPtr
CFStringGetPascalStringPtr (CFStringRef theString, CFStringEncoding encoding);
/** \} */
/** \} */



/** \defgroup CFMutableStringRef CFMutableString Reference
 *  \{
 */
#if OS_API_VERSION(MAC_OS_X_VERSION_10_2, GS_API_LATEST)
typedef enum
{
  kCFStringNormalizationFormD = 0,
  kCFStringNormalizationFormKD = 1,
  kCFStringNormalizationFormC = 2,
  kCFStringNormalizationFormKC = 3
} CFStringNormalizationForm;
#endif

#if OS_API_VERSION(MAC_OS_X_VERSION_10_4, GS_API_LATEST)
CF_EXPORT const CFStringRef kCFStringTransformStripCombiningMarks;
CF_EXPORT const CFStringRef kCFStringTransformToLatin;
CF_EXPORT const CFStringRef kCFStringTransformFullwidthHalfwidth;
CF_EXPORT const CFStringRef kCFStringTransformLatinKatakana;
CF_EXPORT const CFStringRef kCFStringTransformLatinHiragana;
CF_EXPORT const CFStringRef kCFStringTransformHiraganaKatakana;
CF_EXPORT const CFStringRef kCFStringTransformMandarinLatin;
CF_EXPORT const CFStringRef kCFStringTransformLatinHangul;
CF_EXPORT const CFStringRef kCFStringTransformLatinArabic;
CF_EXPORT const CFStringRef kCFStringTransformLatinHebrew;
CF_EXPORT const CFStringRef kCFStringTransformLatinThai;
CF_EXPORT const CFStringRef kCFStringTransformLatinCyrillic;
CF_EXPORT const CFStringRef kCFStringTransformLatinGreek;
CF_EXPORT const CFStringRef kCFStringTransformToXMLHex;
CF_EXPORT const CFStringRef kCFStringTransformToUnicodeName;
#endif
#if OS_API_VERSION(MAC_OS_X_VERSION_10_5, GS_API_LATEST)
CF_EXPORT const CFStringRef kCFStringTransformStripDiacritics;
#endif

CF_EXPORT void
CFStringAppend (CFMutableStringRef theString, CFStringRef appendedString);

CF_EXPORT void
CFStringAppendCharacters (CFMutableStringRef theString,
  const UniChar *chars, CFIndex numChars);

CF_EXPORT void
CFStringAppendCString (CFMutableStringRef theString, const char *cStr,
  CFStringEncoding encoding);

CF_EXPORT void
CFStringAppendFormat (CFMutableStringRef theString,
  CFDictionaryRef formatOptions, CFStringRef format, ...);

CF_EXPORT void
CFStringAppendFormatAndArguments (CFMutableStringRef theString,
  CFDictionaryRef formatOptions, CFStringRef format, va_list arguments);

CF_EXPORT void
CFStringAppendPascalString (CFMutableStringRef theString,
  ConstStr255Param pStr, CFStringEncoding encoding);

CF_EXPORT void
CFStringCapitalize (CFMutableStringRef theString, CFLocaleRef locale);

CF_EXPORT CFMutableStringRef
CFStringCreateMutable (CFAllocatorRef alloc, CFIndex maxLength);

CF_EXPORT CFMutableStringRef
CFStringCreateMutableCopy (CFAllocatorRef alloc, CFIndex maxLength,
  CFStringRef theString);

CF_EXPORT CFMutableStringRef
CFStringCreateMutableWithExternalCharactersNoCopy (CFAllocatorRef alloc,
  UniChar *chars, CFIndex numChars, CFIndex capacity,
  CFAllocatorRef externalCharactersAllocator);

CF_EXPORT void
CFStringDelete (CFMutableStringRef theString, CFRange range);

CF_EXPORT void
CFStringInsert (CFMutableStringRef str, CFIndex idx, CFStringRef insertedStr);

CF_EXPORT void
CFStringLowercase (CFMutableStringRef theString, CFLocaleRef locale);

CF_EXPORT void
CFStringPad (CFMutableStringRef theString, CFStringRef padString,
  CFIndex length, CFIndex indexIntoPad);

CF_EXPORT void
CFStringReplace (CFMutableStringRef theString, CFRange range,
  CFStringRef replacement);

CF_EXPORT void
CFStringReplaceAll (CFMutableStringRef theString, CFStringRef replacement);

CF_EXPORT void
CFStringSetExternalCharactersNoCopy (CFMutableStringRef theString,
  UniChar *chars, CFIndex length, CFIndex capacity);

CF_EXPORT void
CFStringTrim (CFMutableStringRef theString, CFStringRef trimString);

CF_EXPORT void
CFStringTrimWhitespace (CFMutableStringRef theString);

CF_EXPORT void
CFStringUppercase (CFMutableStringRef theString, CFLocaleRef locale);

#if OS_API_VERSION(MAC_OS_X_VERSION_10_2, GS_API_LATEST)
CF_EXPORT CFIndex
CFStringFindAndReplace (CFMutableStringRef theString,
  CFStringRef stringToFind, CFStringRef replacementString,
  CFRange rangeToSearch, CFOptionFlags compareOptions);

CF_EXPORT void
CFStringNormalize (CFMutableStringRef theString,
  CFStringNormalizationForm theForm);
#endif

#if OS_API_VERSION(MAC_OS_X_VERSION_10_4, GS_API_LATEST)
CF_EXPORT Boolean
CFStringTransform (CFMutableStringRef string, CFRange *range,
  CFStringRef transform, Boolean reverse);
#endif

#if OS_API_VERSION(MAC_OS_X_VERSION_10_5, GS_API_LATEST)
CF_EXPORT void
CFStringFold (CFMutableStringRef theString, CFOptionFlags theFlags,
  CFLocaleRef theLocale);
#endif
/** \} */



/** \ingroup CFStringRef
    \name CFStringInlineBuffer
    \{
 */
#define __kCFStringInlineBufferLength 64
struct CFStringInlineBuffer
{
  UniChar buffer[__kCFStringInlineBufferLength];
  CFStringRef theString;
  const UniChar *directBuffer;
  CFRange rangeToBuffer;
  CFIndex bufferedRangeStart;
  CFIndex bufferedRangeEnd;
};
typedef struct CFStringInlineBuffer CFStringInlineBuffer;

CF_INLINE void
CFStringInitInlineBuffer (CFStringRef str, CFStringInlineBuffer *buf,
  CFRange range)
{
  buf->theString = str;
  buf->rangeToBuffer = range;
  buf->directBuffer = CFStringGetCharactersPtr (str);
  buf->bufferedRangeStart = 0;
  buf->bufferedRangeEnd = 0;
}

CF_INLINE UniChar
CFStringGetCharacterFromInlineBuffer (CFStringInlineBuffer *buf, CFIndex idx)
{
  if (buf->directBuffer)
    {
      if (idx < 0 || idx >= buf->rangeToBuffer.length)
        return 0;
      return buf->directBuffer[idx + buf->rangeToBuffer.location];
    }
  else if (idx >= buf->bufferedRangeEnd || idx < buf->bufferedRangeStart)
    {
      CFRange range;
      
      if (idx < 0 || idx >= buf->rangeToBuffer.length)
        return 0;
      
      /* Use 16 here so it's efficient to go backwards, too */
      buf->bufferedRangeStart = idx - 16;
      if (buf->bufferedRangeStart < 0)
        buf->bufferedRangeStart = 0;
      buf->bufferedRangeEnd =
        buf->bufferedRangeStart + __kCFStringInlineBufferLength;
      if (buf->bufferedRangeEnd > buf->rangeToBuffer.length)
        buf->bufferedRangeEnd = buf->rangeToBuffer.length;
      
      range = CFRangeMake (buf->rangeToBuffer.location + buf->bufferedRangeStart,
        buf->bufferedRangeEnd - buf->bufferedRangeStart);
      
      CFStringGetCharacters (buf->theString, range, buf->buffer);
    }
  
  return buf->buffer[(idx - buf->bufferedRangeStart)];
}
/** \} */

CF_EXTERN_C_END

#endif /* __COREFOUNDATION_CFSTRING_H__ */

