/** Declaration of extension methods for base additions

   Copyright (C) 2003-2010 Free Software Foundation, Inc.

   Written by:  Richard Frith-Macdonald <rfm@gnu.org>
   and:         Adam Fedor <fedor@gnu.org>

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

#ifndef	INCLUDED_NSArray_GNUstepBase_h
#define	INCLUDED_NSArray_GNUstepBase_h

#import "../GNUstepBase/GSVersionMacros.h"
#import "../Foundation/NSArray.h"

#if	defined(__cplusplus)
extern "C" {
#endif

#if	OS_API_VERSION(GS_API_NONE,GS_API_LATEST)

@interface NSArray (GNUstepBase)

/** <p>Method for working with sorted arrays - use a binary chop
 * to determine the insertion location for an object.  If equal objects
 * already exist in the array, they will be located immediately before
 * the insertion position.
 * </p>
 * <p>The comparator function takes two items as arguments, the first is the
 * item to be added, the second is the item already in the array.
 * The function should return NSOrderedAscending if the item to be
 * added is 'less than' the item in the array, NSOrderedDescending
 * if it is greater, and NSOrderedSame if it is equal.
 * </p>
 */
- (NSUInteger) insertionPosition: (id)item
		   usingFunction: (NSComparisonResult (*)(id, id, void *))sorter
		         context: (void *)context;

/* <p>Method for working with sorted arrays - use a binary chop
 * to determine the insertion location for an object.  If equal objects
 * already exist in the array, they will be located immediately before
 * the insertion position.
 * </p> 
 * <p>The selector identifies a method returning NSOrderedAscending if
 * the receiver is 'less than' the argument, and NSOrderedDescending if
 * it is greate.
 * </p>
 */
- (NSUInteger) insertionPosition: (id)item
		   usingSelector: (SEL)comp;
@end

#endif	/* OS_API_VERSION */

#if	defined(__cplusplus)
}
#endif

#endif	/* INCLUDED_NSArray_GNUstepBase_h */

