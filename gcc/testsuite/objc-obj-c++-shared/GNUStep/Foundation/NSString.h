/* Interface for NSString for GNUstep
   Copyright (C) 1995, 1996, 1999 Free Software Foundation, Inc.

   Written by:  Andrew Kachites McCallum <mccallum@gnu.ai.mit.edu>
   Date: 1995
   
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

/**
<chapter>
 <heading>Portable path handling</heading>
 <p>Portable path handling (across both unix-like and mswindows operating
 systems) requires some care.  A modern operating system uses the concept
 of a single root to the filesystem, but mswindows has multiple filesystems
 with no common root, so code must be aware of this.  There is also the
 more minor issue that windows often uses a backslash as a separator between
 the components of a path and unix-like systems always use forward slash.<br />
 On windows there is also the issue that two styles of path are used,
 most commonly with a drive letter and a path on that drive
 (eg. 'C:\directory\file') but also UNC paths
 (eg. '//host/share/directory/file') so path handling functions must deal
 with both formats.
 </p>
 <p>GNUstep has three path handling modes, 'gnustep', 'unix', and 'windows'.
 The mode defaults to 'gnustep' but may be set using the GSPathHandling()
 function.<br />
 You should probably stick to using the default 'gnustep' mode in which the
 path handling methods cope with both 'unix' and 'windows' style paths in
 portable and tolerant manner:<br />
 Paths are read in literally so they can be in the native format provided
 by the operating system or in a non-native format. See
 [NSFileManager-stringWithFileSystemRepresentation:length:].<br />
 Paths are written out using the native format of the system the application
 is running on (eg on windows slashes are converted to backslashes).
 See [NSFileManager-fileSystemRepresentationWithPath:].<br />
 The path handling methods accept either a forward or backward slash as a
 path separator when parsing any path.<br />
 Unless operating in 'unix' mode, a leading letter followed by a colon is
 considered the start of a windows style path (the drive specifier), and a
 path beginning with something of the form '//host/share/' is considered
 the start of a UNC style path.<br />
 The path handling methods add forward slashes when building new paths
 internally or when standardising paths, so those path strings provide
 a portable representation (as long as they are relative paths, not including
 system specific roots).<br />
 An important case to note is that on windows a path which looks at first
 glance like an absolute path may actually be a relative one.<br />
 'C:file' is a relative path because it specifies  a file on the C drive
 but does not say what directory it is in.<br />
Similarly, '/dir/file' is a relative path because it specifies the full
location fo a file on a drive, but does not specify which drive it is on.
 </p>
<p>As a consequence of this path handling, you are able to work completely
portably using relative paths (adding components, extensions and
relative paths to a pth, or removing components, extensions and relative
paths from a path etc), and when you save paths as strings in files
which may be transferred to another platform, you should save a relative
path.<br />
When you need to know absolute paths of various points in the filesystem,
you can use various path utility functions to obtain those absolute paths.
For instance, instead of saving an absolute path to a file, you might want
to save a path relative to a user's home directory.  You could do that by
calling NSHomeDirectory() to get the home directory, and only saving the
part of the full path after that prefix.
</p>
</chapter>
 */ 

#ifndef __NSString_h_GNUSTEP_BASE_INCLUDE
#define __NSString_h_GNUSTEP_BASE_INCLUDE
#import	"../GNUstepBase/GSVersionMacros.h"

#import	"NSObject.h"
#import	"NSRange.h"

#if	defined(__cplusplus)
extern "C" {
#endif

/**
 * Type for representing unicode characters.  (16-bit)
 */
typedef uint16_t unichar;

#if OS_API_VERSION(MAC_OS_X_VERSION_10_5,GS_API_LATEST) 
#define NSMaximumStringLength   (INT_MAX-1)
#endif

@class NSArray;
@class NSCharacterSet;
@class NSData;
@class NSDictionary;
#if OS_API_VERSION(GS_API_MACOSX, GS_API_LATEST)
@class NSError;
@class NSLocale;
@class NSURL;
#endif

#define NSMaximumStringLength	(INT_MAX-1)

enum 
{
  NSCaseInsensitiveSearch = 1,
  NSLiteralSearch = 2,
  NSBackwardsSearch = 4,
  NSAnchoredSearch = 8,
  NSNumericSearch = 64	/* MacOS-X 10.2 */
#if OS_API_VERSION(MAC_OS_X_VERSION_10_5,GS_API_LATEST) 
 ,
 NSDiacriticInsensitiveSearch = 128,
 NSWidthInsensitiveSearch = 256,
 NSForcedOrderingSearch = 512
#endif
#if OS_API_VERSION(MAC_OS_X_VERSION_10_7,GS_API_LATEST) 
 ,
 /**
  * Treats the search string as a regular expression.  This option may be
  * combined with NSCaseInsensitiveSearch and NSAnchoredSearch, but no other
  * search options.
  *
  * This option may only be used with the -rangeOfString: family of methods.
  */
 NSRegularExpressionSearch = 1024
#endif
};
typedef NSUInteger NSStringCompareOptions;

/**
 *  <p>Enumeration of available encodings for converting between bytes and
 *  characters (in [NSString]s).  The ones that are shared with OpenStep and
 *  Cocoa are: <code>NSASCIIStringEncoding, NSNEXTSTEPStringEncoding,
 *  NSJapaneseEUCStringEncoding, NSUTF8StringEncoding,
 *  NSISOLatin1StringEncoding, NSSymbolStringEncoding,
 *  NSNonLossyASCIIStringEncoding, NSShiftJISStringEncoding,
 *  NSISOLatin2StringEncoding, NSUnicodeStringEncoding,
 *  NSWindowsCP1251StringEncoding, NSWindowsCP1252StringEncoding,
 *  NSWindowsCP1253StringEncoding, NSWindowsCP1254StringEncoding,
 *  NSWindowsCP1250StringEncoding, NSISO2022JPStringEncoding,
 *  NSMacOSRomanStringEncoding, NSProprietaryStringEncoding</code>.</p>
 *  
 *  <p>Additional encodings available under GNUstep are:
 *  <code>NSKOI8RStringEncoding, NSISOLatin3StringEncoding,
 *  NSISOLatin4StringEncoding, NSISOCyrillicStringEncoding,
 *  NSISOArabicStringEncoding, NSISOGreekStringEncoding,
 *  NSISOHebrewStringEncoding, NSISOLatin5StringEncoding,
 *  NSISOLatin6StringEncoding, NSISOThaiStringEncoding,
 *  NSISOLatin7StringEncoding, NSISOLatin8StringEncoding,
 *  NSISOLatin9StringEncoding, NSGB2312StringEncoding, NSUTF7StringEncoding,
 *  NSGSM0338StringEncoding, NSBIG5StringEncoding,
 *  NSKoreanEUCStringEncoding</code>.</p>
 */
typedef enum _NSStringEncoding
{
/* NB. Must not have an encoding with value zero - so we can use zero to
   tell that a variable that should contain an encoding has not yet been
   initialised */
  GSUndefinedEncoding = 0,
  NSASCIIStringEncoding = 1,
  NSNEXTSTEPStringEncoding = 2,
  NSJapaneseEUCStringEncoding = 3,
  NSUTF8StringEncoding = 4,
  NSISOLatin1StringEncoding = 5,	// ISO-8859-1; West European
  NSSymbolStringEncoding = 6,
  NSNonLossyASCIIStringEncoding = 7,
  NSShiftJISStringEncoding = 8,
  NSISOLatin2StringEncoding = 9,	// ISO-8859-2; East European
  NSUnicodeStringEncoding = 10,
  NSUTF16StringEncoding = NSUnicodeStringEncoding,      // An alias
  NSWindowsCP1251StringEncoding = 11,
  NSWindowsCP1252StringEncoding = 12,	// WinLatin1
  NSWindowsCP1253StringEncoding = 13,	// Greek
  NSWindowsCP1254StringEncoding = 14,	// Turkish
  NSWindowsCP1250StringEncoding = 15,	// WinLatin2
  NSISO2022JPStringEncoding = 21,
  NSMacOSRomanStringEncoding = 30,
  NSProprietaryStringEncoding = 31,

  NSKOI8RStringEncoding = 50,		// Russian/Cyrillic
  NSISOLatin3StringEncoding = 51,	// ISO-8859-3; South European
  NSISOLatin4StringEncoding = 52,	// ISO-8859-4; North European
  NSISOCyrillicStringEncoding = 22,	// ISO-8859-5
  NSISOArabicStringEncoding = 53,	// ISO-8859-6
  NSISOGreekStringEncoding = 54,	// ISO-8859-7
  NSISOHebrewStringEncoding = 55,	// ISO-8859-8
  NSISOLatin5StringEncoding = 57,	// ISO-8859-9; Turkish
  NSISOLatin6StringEncoding = 58,	// ISO-8859-10; Nordic
  NSISOThaiStringEncoding = 59,		// ISO-8859-11
/* Possible future ISO-8859 additions
					// ISO-8859-12
*/
  NSISOLatin7StringEncoding = 61,	// ISO-8859-13
  NSISOLatin8StringEncoding = 62,	// ISO-8859-14
  NSISOLatin9StringEncoding = 63,	// ISO-8859-15; Replaces ISOLatin1
  NSGB2312StringEncoding = 56,
  NSUTF7StringEncoding = 64,		// RFC 2152
  NSGSM0338StringEncoding,		// GSM (mobile phone) default alphabet
  NSBIG5StringEncoding,			// Traditional chinese
  NSKoreanEUCStringEncoding		// Korean

#if OS_API_VERSION(MAC_OS_X_VERSION_10_4,GS_API_LATEST) 
  ,
  NSUTF16BigEndianStringEncoding = 0x90000100,
  NSUTF16LittleEndianStringEncoding = 0x94000100,
  NSUTF32StringEncoding = 0x8c000100,
  NSUTF32BigEndianStringEncoding = 0x98000100,
  NSUTF32LittleEndianStringEncoding = 0x9c000100
#endif
} NSStringEncoding;

enum {
  NSOpenStepUnicodeReservedBase = 0xF400
};

#if OS_API_VERSION(MAC_OS_X_VERSION_10_4,GS_API_LATEST) 
enum {
  NSStringEncodingConversionAllowLossy = 1,
  NSStringEncodingConversionExternalRepresentation = 2
};
typedef NSUInteger NSStringEncodingConversionOptions;
#endif

/**
 * <p>
 *   <code>NSString</code> objects represent an immutable string of Unicode 3.0
 *   characters.  These may be accessed individually as type
 *   <code>unichar</code>, an unsigned short.<br/>
 *   The [NSMutableString] subclass represents a modifiable string.  Both are
 *   implemented as part of a class cluster and the instances you receive may
 *   actually be of unspecified concrete subclasses.
 * </p>
 * <p>
 *   A constant <code>NSString</code> can be created using the following syntax:
 *   <code>@"..."</code>, where the contents of the quotes are the
 *   string, using only ASCII characters.
 * </p>
 * <p>
 *   A variable string can be created using a C printf-like <em>format</em>,
 *   as in <code>[NSString stringWithFormat: @"Total is %f", t]</code>.
 * </p>
 * <p>
 *   To create a concrete subclass of <code>NSString</code>, you must have your
 *   class inherit from <code>NSString</code> and override at least the two
 *   primitive methods - -length and -characterAtIndex:
 * </p>
 * <p>
 *   In general the rule is that your subclass must override any
 *   initialiser that you want to use with it.  The GNUstep
 *   implementation relaxes that to say that, you may override
 *   only the <em>designated initialiser</em> and the other
 *   initialisation methods should work.
 * </p>
 * <p>
 *   Where an NSString instance method returns an NSString object,
 *   the class of the actual object returned may be any subclass
 *   of NSString.  The actual value returned may be a new
 *   autoreleased object, an autoreleased copy of the receiver,
 *   or the receiver itsself.  While the abstract base class
 *   implementations of methods (other than initialisers) will
 *   avoid returning mutable strings by returning an autoreleased
 *   copy of a mutable receiver, concrete subclasses may behave
 *   differently, so code should not rely upon the mutability of
 *   returned strings nor upon their lifetime being greater than
 *   that of the receiver which returned them.
 * </p>
 */
@interface NSString :NSObject <NSCoding, NSCopying, NSMutableCopying>

+ (id) string;
+ (id) stringWithCharacters: (const unichar*)chars
		     length: (NSUInteger)length;
#if OS_API_VERSION(MAC_OS_X_VERSION_10_4,GS_API_LATEST) && GS_API_VERSION( 10200,GS_API_LATEST)
+ (id) stringWithCString: (const char*)byteString
		encoding: (NSStringEncoding)encoding;
#endif
+ (id) stringWithCString: (const char*)byteString
		  length: (NSUInteger)length;
+ (id) stringWithCString: (const char*)byteString;
+ (id) stringWithFormat: (NSString*)format, ... NS_FORMAT_FUNCTION(1,2);
+ (id) stringWithContentsOfFile:(NSString *)path;

// Initializing Newly Allocated Strings
- (id) init;
#if OS_API_VERSION(MAC_OS_X_VERSION_10_4,GS_API_LATEST) && GS_API_VERSION( 10200,GS_API_LATEST)
- (id) initWithBytes: (const void*)bytes
	      length: (NSUInteger)length
	    encoding: (NSStringEncoding)encoding;
- (id) initWithBytesNoCopy: (void*)bytes
		    length: (NSUInteger)length
		  encoding: (NSStringEncoding)encoding 
	      freeWhenDone: (BOOL)flag;
#endif
#if OS_API_VERSION(MAC_OS_X_VERSION_10_4,GS_API_LATEST)
+ (id) stringWithContentsOfFile: (NSString*)path
                   usedEncoding: (NSStringEncoding*)enc
                          error: (NSError**)error;
- (id) initWithContentsOfFile: (NSString*)path
                 usedEncoding: (NSStringEncoding*)enc
                        error: (NSError**)error;
+ (id) stringWithContentsOfFile: (NSString*)path
                       encoding: (NSStringEncoding)enc
                          error: (NSError**)error;
- (id) initWithContentsOfFile: (NSString*)path
                     encoding: (NSStringEncoding)enc
                        error: (NSError**)error;
+ (id) stringWithContentsOfURL: (NSURL*)url
                  usedEncoding: (NSStringEncoding*)enc
                         error: (NSError**)error;
- (id) initWithContentsOfURL: (NSURL*)url
                usedEncoding: (NSStringEncoding*)enc
                       error: (NSError**)error;
+ (id) stringWithContentsOfURL: (NSURL*)url
                      encoding: (NSStringEncoding)enc
                         error: (NSError**)error;
- (id) initWithContentsOfURL: (NSURL*)url
                    encoding: (NSStringEncoding)enc
                       error: (NSError**)error;
- (BOOL) writeToFile: (NSString*)path
	  atomically: (BOOL)atomically
	    encoding: (NSStringEncoding)enc
	       error: (NSError**)error;
- (BOOL) writeToURL: (NSURL*)url
	 atomically: (BOOL)atomically
	   encoding: (NSStringEncoding)enc
	      error: (NSError**)error;
#endif
#if OS_API_VERSION(MAC_OS_X_VERSION_10_5,GS_API_LATEST)
- (NSString*)stringByReplacingOccurrencesOfString: (NSString*)replace
                                       withString: (NSString*)by
                                          options: (NSStringCompareOptions)opts
                                            range: (NSRange)searchRange;
- (NSString*)stringByReplacingOccurrencesOfString: (NSString*)replace
                                       withString: (NSString*)by;
- (NSString*) stringByReplacingCharactersInRange: (NSRange)aRange 
                                      withString: (NSString*)by;
#endif
- (id) initWithCharactersNoCopy: (unichar*)chars
			 length: (NSUInteger)length
		   freeWhenDone: (BOOL)flag;
- (id) initWithCharacters: (const unichar*)chars
		   length: (NSUInteger)length;
- (id) initWithCStringNoCopy: (char*)byteString
		      length: (NSUInteger)length
	        freeWhenDone: (BOOL)flag;
- (id) initWithCString: (const char*)byteString
	        length: (NSUInteger)length;
- (id) initWithCString: (const char*)byteString;
- (id) initWithString: (NSString*)string;
- (id) initWithFormat: (NSString*)format, ... NS_FORMAT_FUNCTION(1,2);
- (id) initWithFormat: (NSString*)format
	    arguments: (va_list)argList NS_FORMAT_FUNCTION(1,0);
- (id) initWithData: (NSData*)data
	   encoding: (NSStringEncoding)encoding;
- (id) initWithContentsOfFile: (NSString*)path;

// Getting a String's Length
- (NSUInteger) length;

// Accessing Characters
- (unichar) characterAtIndex: (NSUInteger)index;
- (void) getCharacters: (unichar*)buffer;
- (void) getCharacters: (unichar*)buffer
		 range: (NSRange)aRange;

// Combining Strings
- (NSString*) stringByAppendingFormat: (NSString*)format, ...
  NS_FORMAT_FUNCTION(1,2);
- (NSString*) stringByAppendingString: (NSString*)aString;

// Dividing Strings into Substrings
- (NSArray*) componentsSeparatedByString: (NSString*)separator;
- (NSString*) substringFromIndex: (NSUInteger)index;
- (NSString*) substringToIndex: (NSUInteger)index;

// Finding Ranges of Characters and Substrings
- (NSRange) rangeOfCharacterFromSet: (NSCharacterSet*)aSet;
- (NSRange) rangeOfCharacterFromSet: (NSCharacterSet*)aSet
			    options: (NSUInteger)mask;
- (NSRange) rangeOfCharacterFromSet: (NSCharacterSet*)aSet
			    options: (NSUInteger)mask
			      range: (NSRange)aRange;
- (NSRange) rangeOfString: (NSString*)string;
- (NSRange) rangeOfString: (NSString*)string
		  options: (NSUInteger)mask;
- (NSRange) rangeOfString: (NSString*)aString
		  options: (NSUInteger)mask
		    range: (NSRange)aRange;

// Determining Composed Character Sequences
- (NSRange) rangeOfComposedCharacterSequenceAtIndex: (NSUInteger)anIndex;

#if OS_API_VERSION(MAC_OS_X_VERSION_10_2,GS_API_LATEST) 
/** Returns a copy of the receiver normalised using the KD form.
 */
- (NSString *) decomposedStringWithCompatibilityMapping;

/** Returns a copy of the receiver normalised using the D form.
 */
- (NSString *) decomposedStringWithCanonicalMapping;

/** Returns a copy of the receiver normalised using the KC form.
 */
- (NSString *) precomposedStringWithCompatibilityMapping;

/** Returns a copy of the receiver normalised using the C form.
 */
- (NSString *) precomposedStringWithCanonicalMapping;
#endif

// Converting String Contents into a Property List
- (id) propertyList;
- (NSDictionary*) propertyListFromStringsFileFormat;

// Identifying and Comparing Strings
- (NSComparisonResult) compare: (NSString*)aString;
- (NSComparisonResult) compare: (NSString*)aString	
		       options: (NSUInteger)mask;
- (NSComparisonResult) compare: (NSString*)aString
		       options: (NSUInteger)mask
			 range: (NSRange)aRange;
- (BOOL) hasPrefix: (NSString*)aString;
- (BOOL) hasSuffix: (NSString*)aString;
- (BOOL) isEqual: (id)anObject;
- (BOOL) isEqualToString: (NSString*)aString;
- (NSUInteger) hash;

// Getting a Shared Prefix
- (NSString*) commonPrefixWithString: (NSString*)aString
			     options: (NSUInteger)mask;

// Changing Case
- (NSString*) capitalizedString;
- (NSString*) lowercaseString;
- (NSString*) uppercaseString;

// Getting C Strings
- (const char*) cString;
#if OS_API_VERSION(GS_API_MACOSX, GS_API_LATEST)

#if OS_API_VERSION(MAC_OS_X_VERSION_10_4,GS_API_LATEST) && GS_API_VERSION( 10200,GS_API_LATEST)
- (const char*) cStringUsingEncoding: (NSStringEncoding)encoding;
- (BOOL) getCString: (char*)buffer
	  maxLength: (NSUInteger)maxLength
	   encoding: (NSStringEncoding)encoding;
- (id) initWithCString: (const char*)byteString
	      encoding: (NSStringEncoding)encoding;
- (NSUInteger) lengthOfBytesUsingEncoding: (NSStringEncoding)encoding;
- (NSUInteger) maximumLengthOfBytesUsingEncoding: (NSStringEncoding)encoding;
#endif

#endif
- (NSUInteger) cStringLength;
- (void) getCString: (char*)buffer;
- (void) getCString: (char*)buffer
	  maxLength: (NSUInteger)maxLength;
- (void) getCString: (char*)buffer
	  maxLength: (NSUInteger)maxLength
	      range: (NSRange)aRange
     remainingRange: (NSRange*)leftoverRange;

// Getting Numeric Values
- (float) floatValue;
- (int) intValue;

// Working With Encodings
- (BOOL) canBeConvertedToEncoding: (NSStringEncoding)encoding;
- (NSData*) dataUsingEncoding: (NSStringEncoding)encoding;
- (NSData*) dataUsingEncoding: (NSStringEncoding)encoding
	 allowLossyConversion: (BOOL)flag;
+ (NSStringEncoding) defaultCStringEncoding;
- (NSString*) description;
- (NSStringEncoding) fastestEncoding;
- (NSStringEncoding) smallestEncoding;

/**
 * Attempts to complete this string as a path in the filesystem by finding
 * a unique completion if one exists and returning it by reference in
 * outputName (which must be a non-nil pointer), or if it finds a set of
 * completions they are returned by reference in outputArray, if it is non-nil.
 * filterTypes can be an array of strings specifying extensions to consider;
 * files without these extensions will be ignored and will not constitute
 * completions.  Returns 0 if no match found, else a positive number that is
 * only accurate if outputArray was non-nil.
 */
- (NSUInteger) completePathIntoString: (NSString**)outputName
			caseSensitive: (BOOL)flag
		     matchesIntoArray: (NSArray**)outputArray
			  filterTypes: (NSArray*)filterTypes;

/**
 * Converts the receiver to a C string path expressed in the character
 * encoding appropriate for the local host file system.  This string will be
 * automatically freed soon after it is returned, so copy it if you need it
 * for long.<br />
 * NB. On mingw32 systems the filesystem representation of a path is a 16-bit
 * unicode character string, so you should only pass the value returned by
 * this method to functions expecting wide characters.<br />
 * This method uses [NSFileManager-fileSystemRepresentationWithPath:] to
 * perform the conversion.
 */
- (const GSNativeChar*) fileSystemRepresentation;

/**
 * Converts the receiver to a C string path using the character encoding
 * appropriate to the local file system.  This string will be stored
 * into buffer if it is shorter (number of characters) than size,
 * otherwise NO is returned.<br />
 * NB. On mingw32 systems the filesystem representation of a path is a 16-bit
 * unicode character string, so the buffer you pass to this method must be
 * twice as many bytes as the size (number of characters) you expect to
 * receive.<br />
 * This method uses [NSFileManager-fileSystemRepresentationWithPath:] to
 * perform the conversion.
 */
- (BOOL) getFileSystemRepresentation: (GSNativeChar*)buffer
			   maxLength: (NSUInteger)size;

/**
 * Returns a string containing the last path component of the receiver.<br />
 * The path component is the last non-empty substring delimited by the ends
 * of the string, or by path separator characters.<br />
 * If the receiver only contains a root part, this method returns it.<br />
 * If there are no non-empty substrings, this returns an empty string.<br />
 * NB. In a windows UNC path, the host and share specification is treated as
 * a single path component, even though it contains separators.
 * So a string of the form '//host/share' may be returned.<br />
 * Other special cases are apply when the string is the root.
 * <example>
 *   @"foo/bar" produces @"bar"
 *   @"foo/bar/" produces @"bar"
 *   @"/foo/bar" produces @"bar"
 *   @"/foo" produces @"foo"
 *   @"/" produces @"/" (root is a special case)
 *   @"" produces @""
 *   @"C:/" produces @"C:/" (root is a special case)
 *   @"C:" produces @"C:"
 *   @"//host/share/" produces @"//host/share/" (root is a special case)
 *   @"//host/share" produces @"//host/share"
 * </example>
 */
- (NSString*) lastPathComponent;

/**
 * Returns a new string containing the path extension of the receiver.<br />
 * The path extension is a suffix on the last path component which starts
 * with the extension separator (a '.') (for example .tiff is the
 * pathExtension for /foo/bar.tiff).<br />
 * Returns an empty string if no such extension exists.
 * <example>
 *   @"a.b" produces @"b"
 *   @"a.b/" produces @"b"
 *   @"/path/a.ext" produces @"ext"
 *   @"/path/a." produces @""
 *   @"/path/.a" produces @"" (.a is not an extension to a file)
 *   @".a" produces @"" (.a is not an extension to a file)
 * </example>
 */
- (NSString*) pathExtension;

/**
 * Returns a string where a prefix of the current user's home directory is
 * abbreviated by '~', or returns the receiver (or an immutable copy) if
 * it was not found to have the home directory as a prefix.
 */
- (NSString*) stringByAbbreviatingWithTildeInPath;

/**
 * Returns a new string with the path component given in aString
 * appended to the receiver.<br />
 * This removes trailing path separators from the receiver and the root
 * part from aString and replaces them with a single slash as a path
 * separator.<br />
 * Also condenses any multiple separator sequences in the result into
 * single path separators.
 * <example>
 *   @"" with @"file" produces @"file"
 *   @"path" with @"file" produces @"path/file"
 *   @"/" with @"file" produces @"/file"
 *   @"/" with @"file" produces @"/file"
 *   @"/" with @"/file" produces @"/file"
 *   @"path with @"C:/file" produces @"path/file"
 * </example>
 * NB. Do not use this method to modify strings other than filesystem
 * paths as the behavior in such cases is undefined ... for instance
 * the string may have repeated slashes or slash-dot-slash sequences
 * removed.
 */
- (NSString*) stringByAppendingPathComponent: (NSString*)aString;

/**
 * Returns a new string with the path extension given in aString
 * appended to the receiver after an extensionSeparator ('.').<br />
 * If the receiver has trailing path separator characters, they are
 * stripped before the extension separator is added.<br />
 * If the receiver contains no components after the root, the extension
 * cannot be appended (an extension can only be appended to a file name),
 * so a copy of the unmodified receiver is returned.<br />
 * An empty string may be used as an extension ... in which case the extension
 * separator is appended.<br />
 * This behavior mirrors that of the -stringByDeletingPathExtension method.
 * <example>
 *   @"Mail" with @"app" produces @"Mail.app"
 *   @"Mail.app" with @"old" produces @"Mail.app.old"
 *   @"file" with @"" produces @"file."
 *   @"/" with @"app" produces @"/" (no file name to append to)
 *   @"" with @"app" produces @"" (no file name to append to)
 * </example>
 * NB. Do not use this method to modify strings other than filesystem
 * paths as the behavior in such cases is undefined ... for instance
 * the string may have repeated slashes or slash-dot-slash sequences
 * removed.
 */
- (NSString*) stringByAppendingPathExtension: (NSString*)aString;

/**
 * Returns a new string with the last path component (including any final
 * path separators) removed from the receiver.<br />
 * A string without a path component other than the root is returned
 * without alteration.<br />
 * See -lastPathComponent for a definition of a path component.
 * <example>
 *   @"hello/there" produces @"hello" (a relative path)
 *   @"hello" produces @"" (a relative path)
 *   @"/hello" produces @"/" (an absolute unix path)
 *   @"/" produces @"/" (an absolute unix path)
 *   @"C:file" produces @"C:" (a relative windows path)
 *   @"C:" produces @"C:" (a relative windows path)
 *   @"C:/file" produces @"C:/" (an absolute windows path)
 *   @"C:/" produces @"C:/" (an absolute windows path)
 *   @"//host/share/file" produces @"//host/share/" (a UNC path)
 *   @"//host/share/" produces @"//host/share/" (a UNC path)
 *   @"//path/file" produces @"//path" (an absolute Unix path)
 * </example>
 * NB. Do not use this method to modify strings other than filesystem
 * paths as the behavior in such cases is undefined ... for instance
 * the string may have repeated slashes or slash-dot-slash sequences
 * removed.
 */
- (NSString*) stringByDeletingLastPathComponent;

/**
 * Returns a new string with the path extension removed from the receiver.<br />
 * Strips any trailing path separators before checking for the extension
 * separator.<br />
 * NB. This method does not consider a string which contains nothing
 * between the root part and the extension separator ('.') to be a path
 * extension. This mirrors the behavior of the -stringByAppendingPathExtension:
 * method.
 * <example>
 *   @"file.ext" produces @"file"
 *   @"/file.ext" produces @"/file"
 *   @"/file.ext/" produces @"/file" (trailing path separators are ignored)
 *   @"/file..ext" produces @"/file."
 *   @"/file." produces @"/file"
 *   @"/.ext" produces @"/.ext" (there is no file to strip from)
 *   @".ext" produces @".ext" (there is no file to strip from)
 * </example>
 * NB. Do not use this method to modify strings other than filesystem
 * paths as the behavior in such cases is undefined ... for instance
 * the string may have repeated slashes or slash-dot-slash sequences
 * removed.
 */
- (NSString*) stringByDeletingPathExtension;

/**
 * Returns a string created by expanding the initial tilde ('~') and any
 * following username to be the home directory of the current user or the
 * named user.<br />
 * Returns the receiver or an immutable copy if it was not possible to
 * expand it.
 */
- (NSString*) stringByExpandingTildeInPath;

/**
 * First calls -stringByExpandingTildeInPath if necessary.<br />
 * Replaces path string by one in which path components representing symbolic
 * links have been replaced by their referents.<br />
 * Removes a leading '/private' if the result is valid.<br />
 * If links cannot be resolved, returns an unmodified copy of the receiver.
 */
- (NSString*) stringByResolvingSymlinksInPath;

/**
 * Returns a standardised form of the receiver, with unnecessary parts
 * removed, tilde characters expanded, and symbolic links resolved
 * where possible.<br />
 * NB. Refers to the local filesystem to resolve symbolic links in
 * absolute paths, and to expand tildes ... so this can't be used for
 * general path manipulation.<br />
 * If the string is an invalid path, the unmodified receiver is returned.<br />
 * <p>
 *   Uses -stringByExpandingTildeInPath to expand tilde expressions.<br />
 *   Simplifies '//' and '/./' sequences and removes trailing '/' or '.'.<br />
 * </p>
 * <p>
 *  For absolute paths, uses -stringByResolvingSymlinksInPath to resolve
 *  any links, then gets rid of '/../' sequences and removes any '/private'
 *  prefix.
 * </p>
 */
- (NSString*) stringByStandardizingPath;


// for methods working with decomposed strings
- (int) _baseLength;

#if OS_API_VERSION(GS_API_MACOSX, GS_API_LATEST)
/**
 * Concatenates the path components in the array and returns the result.<br />
 * This method does not remove empty path components, but does recognize an
 * empty initial component as a special case meaning that the string
 * returned will begin with a slash.
 */
+ (NSString*) pathWithComponents: (NSArray*)components;

/**
 * Returns YES if the receiver represents an absolute path ...<br />
 * Returns NO otherwise.<br />
 * An absolute path in unix mode is one which begins
 * with a slash or tilde.<br />
 * In windows mode a drive specification (eg C:) followed by a slash or
 * backslash, is an absolute path, as is any path beginning with a tilde.<br />
 * In any mode a UNC path (//host/share...) is always absolute.<br />
 * In the default gnustep path handling mode,
 * the rules are the same as for windows,
 * except that a path whose root is a slash denotes an absolute path
 * when running on unix and a relative path when running under windows.
 */
- (BOOL) isAbsolutePath;

/**
 * Returns the path components of the receiver separated into an array.<br />
 * If the receiver begins with a root sequence such as the path separator
 * character (or a drive specification in windows) then that is used as the
 * first element in the array.<br />
 * Empty components are removed.<br />
 * If a trailing path separator (which was not part of the root) was present,
 * it is added as the last element in the array.
 */
- (NSArray*) pathComponents;

/**
 * Returns an array of strings made by appending the values in paths
 * to the receiver.
 */
- (NSArray*) stringsByAppendingPaths: (NSArray*)paths;

+ (NSString*) localizedStringWithFormat: (NSString*)format, ...
  NS_FORMAT_FUNCTION(1,2);

+ (id) stringWithString: (NSString*)aString;
+ (id) stringWithContentsOfURL: (NSURL*)url;
+ (id) stringWithUTF8String: (const char*)bytes;
- (id) initWithFormat: (NSString*)format
	       locale: (NSDictionary*)locale, ... NS_FORMAT_FUNCTION(1,3);
- (id) initWithFormat: (NSString*)format
	       locale: (NSDictionary*)locale
	    arguments: (va_list)argList NS_FORMAT_FUNCTION(1,0);
- (id) initWithUTF8String: (const char *)bytes;
- (id) initWithContentsOfURL: (NSURL*)url;
- (NSString*) substringWithRange: (NSRange)aRange;
- (NSComparisonResult) caseInsensitiveCompare: (NSString*)aString;
- (NSComparisonResult) compare: (NSString*)string 
		       options: (NSUInteger)mask 
			 range: (NSRange)compareRange 
			locale: (id)locale;
- (NSComparisonResult) localizedCompare: (NSString *)string;
- (NSComparisonResult) localizedCaseInsensitiveCompare: (NSString *)string;
- (BOOL) writeToFile: (NSString*)filename
	  atomically: (BOOL)useAuxiliaryFile;
- (BOOL) writeToURL: (NSURL*)url atomically: (BOOL)atomically;
- (double) doubleValue;
+ (NSStringEncoding*) availableStringEncodings;
+ (NSString*) localizedNameOfStringEncoding: (NSStringEncoding)encoding;
- (void) getLineStart: (NSUInteger *)startIndex
                  end: (NSUInteger *)lineEndIndex
          contentsEnd: (NSUInteger *)contentsEndIndex
             forRange: (NSRange)aRange;
- (NSRange) lineRangeForRange: (NSRange)aRange;
- (const char*) lossyCString;
- (NSString*) stringByAddingPercentEscapesUsingEncoding: (NSStringEncoding)e;
- (NSString*) stringByPaddingToLength: (NSUInteger)newLength
			   withString: (NSString*)padString
		      startingAtIndex: (NSUInteger)padIndex;
- (NSString*) stringByReplacingPercentEscapesUsingEncoding: (NSStringEncoding)e;
- (NSString*) stringByTrimmingCharactersInSet: (NSCharacterSet*)aSet;
- (const char *)UTF8String;
#endif

#if OS_API_VERSION(MAC_OS_X_VERSION_10_9,GS_API_LATEST)
- (NSString *) stringByAddingPercentEncodingWithAllowedCharacters: (NSCharacterSet *)aSet;
- (NSString *) stringByRemovingPercentEncoding;
#endif

#if OS_API_VERSION(MAC_OS_X_VERSION_10_3,GS_API_LATEST) 
/** Not implemented */
- (void) getParagraphStart: (NSUInteger *)startIndex
                       end: (NSUInteger *)parEndIndex
               contentsEnd: (NSUInteger *)contentsEndIndex
                 forRange: (NSRange)range;
/** Not implemented */
 - (NSRange) paragraphRangeForRange: (NSRange)range;
#endif

#if OS_API_VERSION(MAC_OS_X_VERSION_10_5,GS_API_LATEST) 
/**
 * Returns YES when scanning the receiver's text from left to right
 * finds an initial digit in the range 1-9 or a letter in the set
 * ('Y', 'y', 'T', 't').<br />
 * Any trailing characters are ignored.<br />
 * Any leading whitespace or zeros or signs are also ignored.<br />
 * Returns NO if the above conditions are not met.
 */
- (BOOL) boolValue;
- (NSArray *) componentsSeparatedByCharactersInSet: (NSCharacterSet *)separator;
- (NSInteger) integerValue;
- (long long) longLongValue;
/** Not implemented */
- (NSRange) rangeOfComposedCharacterSequencesForRange: (NSRange)range;
/** Not implemented */
- (NSRange) rangeOfString: (NSString *)aString
                  options: (NSStringCompareOptions)mask
                    range: (NSRange)searchRange
                   locale: (NSLocale *)locale;

#endif

#if OS_API_VERSION(MAC_OS_X_VERSION_10_10,GS_API_LATEST) 

/**
  * Returns YES if the receiver contains string, otherwise, NO.
  */
- (BOOL) containsString: (NSString *)string;

#endif

#if OS_API_VERSION(GS_API_NONE, GS_API_NONE)
+ (Class) constantStringClass;
#endif	/* GS_API_NONE */

@end

@interface NSMutableString : NSString

// Creating Temporary Strings
+ (id) string;
+ (id) stringWithCharacters: (const unichar*)characters
		     length: (NSUInteger)length;
+ (id) stringWithCString: (const char*)byteString
		  length: (NSUInteger)length;
+ (id) stringWithCString: (const char*)byteString;
+ (id) stringWithFormat: (NSString*)format, ... NS_FORMAT_FUNCTION(1,2);
+ (id) stringWithContentsOfFile: (NSString*)path;
+ (NSMutableString*) stringWithCapacity: (NSUInteger)capacity;

// Initializing Newly Allocated Strings
- (id) initWithCapacity: (NSUInteger)capacity;

// Modify A String
- (void) appendFormat: (NSString*)format, ... NS_FORMAT_FUNCTION(1,2);
- (void) appendString: (NSString*)aString;
- (void) deleteCharactersInRange: (NSRange)range;
- (void) insertString: (NSString*)aString atIndex: (NSUInteger)loc;
- (void) replaceCharactersInRange: (NSRange)range 
		       withString: (NSString*)aString;
- (NSUInteger) replaceOccurrencesOfString: (NSString*)replace
				 withString: (NSString*)by
				    options: (NSUInteger)opts
				      range: (NSRange)searchRange;
- (void) setString: (NSString*)aString;

@end

#ifdef __OBJC_GNUSTEP_RUNTIME_ABI__
#  if __OBJC_GNUSTEP_RUNTIME_ABI__ >= 20
#    define GNUSTEP_NEW_STRING_ABI
#  endif
#endif

/**
 * <p>The NXConstantString class is used to hold constant 8-bit character
 * string objects produced by the compiler where it sees @"..." in the
 * source.  The compiler generates the instances of this class - which
 * has three instance variables -</p>
 * <list>
 * <item>a pointer to the class (this is the sole ivar of NSObject)</item>
 * <item>a pointer to the 8-bit data</item>
 * <item>the length of the string</item>
 * </list>
 * <p>In older versions of the compiler, the isa variable is always set to
 * the NXConstantString class.  In newer versions a compiler option was
 * added for GNUstep, to permit the isa variable to be set to another
 * class, and GNUstep uses this to avoid conflicts with the default
 * implementation of NXConstantString in the ObjC runtime library (the
 * preprocessor is used to change all occurrences of NXConstantString
 * in the source code to NSConstantString).</p>
 * <p>Since GNUstep will generally use the GNUstep extension to the
 * compiler, you should never refer to the constant string class by
 * name, but should use the [NSString+constantStringClass] method to
 * get the actual class being used for constant strings.</p>
 * What follows is a dummy declaration of the class to keep the compiler
 * happy.
 */
@interface NXConstantString : NSString
{
@public
#ifdef GNUSTEP_NEW_STRING_ABI
  /**
   * Flags.  The low 16 bits are reserved for the compiler, the top 16 for use
   * by the Foundation Framework.  Currently only the low 2 bits are used, to
   * indicate the encoding of the string, with the following values:
   *
   * 0. ASCII (UTF-8 using only 7-bit characters)
   * 1. UTF-8
   * 2. UTF-16
   * 3. UTF-32
   *
   */
  uint32_t flags;
  /**
   * The number of characters (UTF-16 code units) in the string.
   */
  uint32_t nxcslen;
  /**
   * The number of bytes in the string.  For fixed-length encodings, this is a
   * fixed multiple of nxcslen, but for UTF-8 it can be different.
   */
  uint32_t size;
  /**
   * Hash value.
   */
  uint32_t hash;
  /**
   * Pointer to the byte data of the string.  Note that `char*` is the correct
   * type only if the low two bits of the flags indicate that this is an ASCII
   * or UTF-8 string, otherwise it is a pointer to 16- or 32-bit characters in
   * native byte order.
   */
  const char * const nxcsptr;
#else
  const char * const nxcsptr;
  const unsigned int nxcslen;
#endif
}
@end

#ifdef NeXT_RUNTIME
/** For internal use with NeXT runtime;
    needed, until Apple Radar 2870817 is fixed. */
extern struct objc_class _NSConstantStringClassReference;
#endif

#if	defined(__cplusplus)
}
#endif

#if     !NO_GNUSTEP && !defined(GNUSTEP_BASE_INTERNAL)
#import "../GNUstepBase/NSString+GNUstepBase.h"
#import "../GNUstepBase/NSMutableString+GNUstepBase.h"
#endif

#endif /* __NSString_h_GNUSTEP_BASE_INCLUDE */
