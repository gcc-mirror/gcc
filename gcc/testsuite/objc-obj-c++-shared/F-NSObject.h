/* Copyright (C) 2019 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef _OBJC_F_NSOBJECT_H_
#define _OBJC_F_NSOBJECT_H_

/* This is a workaround to PR90709 for the NeXT runtime.
   If we're on a system version that has headers with items we can't
   consume, then use the GNUStep header instead.
*/

#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ >= 1090
#  import "GNUStep/Foundation/NSObject.h"
#else 
#  import <Foundation/NSObject.h>
#endif

#endif /* _OBJC_F_NSOBJECT_H_ */
