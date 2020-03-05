/*
   NSEnumerator.h

   Copyright (C) 1998 Free Software Foundation, Inc.

   Author:  Scott Christley <scottc@net-community.com>
   Date: January 1998

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

#ifndef __NSEnumerator_h_GNUSTEP_BASE_INCLUDE
#define __NSEnumerator_h_GNUSTEP_BASE_INCLUDE
#import	"../GNUstepBase/GSVersionMacros.h"

#import	"NSObject.h"


#if	defined(__cplusplus)
extern "C" {
#endif

@class GS_GENERIC_CLASS(NSArray, ElementT);

typedef struct
{
  unsigned long	state;
  __unsafe_unretained id		*itemsPtr;
  unsigned long	*mutationsPtr;
  unsigned long	extra[5];
} NSFastEnumerationState;

@protocol NSFastEnumeration
- (NSUInteger) countByEnumeratingWithState: (NSFastEnumerationState *)state
				   objects: (__unsafe_unretained id[])stackbuf
				     count: (NSUInteger)len;
@end

@interface GS_GENERIC_CLASS(NSEnumerator, IterT) : NSObject <NSFastEnumeration>
- (GS_GENERIC_CLASS(NSArray, IterT) *) allObjects;
- (GS_GENERIC_TYPE(IterT)) nextObject;
@end

#if	defined(__cplusplus)
}
#endif

#endif /* __NSEnumerator_h_GNUSTEP_BASE_INCLUDE */
