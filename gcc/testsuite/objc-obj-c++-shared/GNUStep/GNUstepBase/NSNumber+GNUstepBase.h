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

#ifndef	INCLUDED_NSNumber_GNUstepBase_h
#define	INCLUDED_NSNumber_GNUstepBase_h

#import "GSVersionMacros.h"
#import "../Foundation/NSValue.h"

#if	defined(__cplusplus)
extern "C" {
#endif

#if	OS_API_VERSION(GS_API_NONE,GS_API_LATEST)

@interface NSNumber(GNUstepBase)
/**
 * Parses string as a <code>double</code>, <code>int</code>, or <code>unsigned
 * int</code> depending on what characters are present.  Uses
 * <code>atof</code> and <code>atoi</code> which don't report errors, so be
 * careful if the string might contain an invalid value.
 */
+ (NSValue*) valueFromString: (NSString *)string;
@end

#endif	/* OS_API_VERSION */

#if	defined(__cplusplus)
}
#endif

#endif	/* INCLUDED_NSNumber_GNUstepBase_h */

