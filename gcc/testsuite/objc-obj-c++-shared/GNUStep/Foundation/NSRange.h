/* 
 * Copyright (C) 1995,1999 Free Software Foundation, Inc.
 * 
 * Written by:  Adam Fedor <fedor@boulder.colorado.edu>
 * Date: 1995
 * 
 * This file is part of the GNUstep Base Library.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02111 USA.
 */ 

#ifndef __NSRange_h_GNUSTEP_BASE_INCLUDE
#define __NSRange_h_GNUSTEP_BASE_INCLUDE
#import	"../GNUstepBase/GSVersionMacros.h"

/**** Included Headers *******************************************************/

#import	"NSObject.h"

#if	defined(__cplusplus)
extern "C" {
#endif

@class NSException;
@class NXConstantString;

/**** Type, Constant, and Macro Definitions **********************************/

#ifndef MAX
#define MAX(a,b) \
       ({__typeof__(a) _MAX_a = (a); __typeof__(b) _MAX_b = (b);  \
         _MAX_a > _MAX_b ? _MAX_a : _MAX_b; })
#define	GS_DEFINED_MAX
#endif

#ifndef MIN
#define MIN(a,b) \
       ({__typeof__(a) _MIN_a = (a); __typeof__(b) _MIN_b = (b);  \
         _MIN_a < _MIN_b ? _MIN_a : _MIN_b; })
#define	GS_DEFINED_MIN
#endif

/**
 * <example>
{
  NSUInteger location;
  NSUInteger length;
}</example>
 * <p>
 *   The NSRange type is used to specify ranges of locations,
 *   typically items in an array, characters in a string, and bytes
 *   in a data object.
 * </p>
 * <p>
 *   As 'boundary' or 'fencepost' errors are a particularly common
 *   problem in programming, it is important that you understand
 *   how an NSRange works.
 * </p>
 * <p>
 *   An NSRange consists of a <em>location</em> and a <em>length</em>.  The
 *   points that are considered to lie in a range are the integers from
 *   the location to the location plus the length, so the number
 *   of points in a range is the length of the range plus one.<br />
 *   However, if you consider these points like the marks on a
 *   ruler, you can only store information <strong>between</strong>
 *   points.  So the number of items that can be stored in a range
 *   is the length of the range.
 * </p>
 */
typedef struct _NSRange NSRange;
struct _NSRange
{
  NSUInteger location;
  NSUInteger length;
};

#if OS_API_VERSION(GS_API_MACOSX, GS_API_LATEST)
/** Pointer to an NSRange structure. */
typedef NSRange *NSRangePointer;
#endif

/**** Function Prototypes ****************************************************/

/*
 *      All but the most complex functions are declared static inline in this
 *      header file so that they are maximally efficient.  In order to provide
 *      true functions (for code modules that don't have this header) this
 *      header is included in NSRange.m where the functions are no longer
 *      declared inline.
 */
#ifdef  IN_NSRANGE_M
#define GS_RANGE_SCOPE   extern
#define GS_RANGE_ATTR
#else
#define GS_RANGE_SCOPE   static inline
#define GS_RANGE_ATTR    __attribute__((unused))
#endif

GS_RANGE_SCOPE NSUInteger
NSMaxRange(NSRange range) GS_RANGE_ATTR;

/** Returns top end of range (location + length). */
GS_RANGE_SCOPE NSUInteger
NSMaxRange(NSRange range) 
{
  return range.location + range.length;
}

GS_RANGE_SCOPE BOOL 
NSLocationInRange(NSUInteger location, NSRange range) GS_RANGE_ATTR;

/** Returns whether location is greater than or equal to range's location
 *  and less than its max.
 */
GS_RANGE_SCOPE BOOL 
NSLocationInRange(NSUInteger location, NSRange range) 
{
  return (location >= range.location) && (location < NSMaxRange(range));
}

/** Convenience method for raising an NSRangeException. */
GS_EXPORT void _NSRangeExceptionRaise (void);
/* NB: The implementation of _NSRangeExceptionRaise is: 
   [NSException raise: NSRangeException
	       format: @"Range location + length too great"]; 

   _NSRangeExceptionRaise is defined in NSRange.m so that this
   file (NSRange.h) can be included without problems in the
   implementation of the base classes themselves. */

GS_RANGE_SCOPE NSRange
NSMakeRange(NSUInteger location, NSUInteger length) GS_RANGE_ATTR;

/** Creates new range starting at location and of given length. */
GS_RANGE_SCOPE NSRange
NSMakeRange(NSUInteger location, NSUInteger length)
{
  NSRange range;
  NSUInteger end = location + length;

  if (end < location || end < length)
    {
      _NSRangeExceptionRaise ();
    }
  range.location = location;
  range.length   = length;
  return range;
}

GS_RANGE_SCOPE BOOL
NSEqualRanges(NSRange range1, NSRange range2) GS_RANGE_ATTR;

/** Returns whether range1 and range2 have same location and length. */
GS_RANGE_SCOPE BOOL
NSEqualRanges(NSRange range1, NSRange range2)
{
  return ((range1.location == range2.location)
                && (range1.length == range2.length));
}

GS_RANGE_SCOPE NSRange
NSUnionRange(NSRange range1, NSRange range2) GS_RANGE_ATTR;

/** Returns range going from minimum of aRange's and bRange's locations to
    maximum of their two max's. */
GS_RANGE_SCOPE NSRange
NSUnionRange(NSRange aRange, NSRange bRange)
{
  NSRange range;

  range.location = MIN(aRange.location, bRange.location);
  range.length   = MAX(NSMaxRange(aRange), NSMaxRange(bRange))
                - range.location;
  return range;
}

GS_RANGE_SCOPE NSRange
NSIntersectionRange(NSRange range1, NSRange range2) GS_RANGE_ATTR;

/** Returns range containing indices existing in both aRange and bRange.  If
 *  the returned length is 0, the location is undefined and should be ignored.
 */
GS_RANGE_SCOPE NSRange
NSIntersectionRange (NSRange aRange, NSRange bRange)
{
  NSRange range;

  if (NSMaxRange(aRange) < bRange.location
                || NSMaxRange(bRange) < aRange.location)
    return NSMakeRange(0, 0);

  range.location = MAX(aRange.location, bRange.location);
  range.length   = MIN(NSMaxRange(aRange), NSMaxRange(bRange))
                - range.location;
  return range;
}


@class NSString;

/** Returns string of form {location=a, length=b}. */
GS_EXPORT NSString *NSStringFromRange(NSRange range);

/** Parses range from string of form {location=a, length=b}; returns range
    with 0 location and length if this fails. */
GS_EXPORT NSRange NSRangeFromString(NSString *aString);

#ifdef	GS_DEFINED_MAX
#undef	GS_DEFINED_MAX
#undef	MAX
#endif

#ifdef	GS_DEFINED_MIN
#undef	GS_DEFINED_MIN
#undef	MIN
#endif

#if	defined(__cplusplus)
}
#endif

#endif /* __NSRange_h_GNUSTEP_BASE_INCLUDE */
