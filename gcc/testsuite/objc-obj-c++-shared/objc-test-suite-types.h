/* Define test-suite types to minimize conditional test-case source.
   Copyright (C) 2011 Free Software Foundation, Inc.
   Contributed by Iain Sandoe 

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

#ifndef _OBJC_TEST_SUITE_TYPES_H_
#define _OBJC_TEST_SUITE_TYPES_H_

#ifndef __NEXT_RUNTIME__

/* dummy const string class ref. */
typedef void * TNS_STRING_REF_T;

#else /* NeXT */

#include "next-abi.h"
#ifdef NEXT_OBJC_USE_NEW_INTERFACE
#include <objc/runtime.h>
#else
#include <objc/objc-runtime.h>
#endif

/* Force a definition of nil that is compatible with GNU runtime.  */
#undef  nil
#define nil ((id)0)

#ifndef NULL
#define NULL 0
#endif

/* Where there are equivalent interfaces between APIs we substitute
   a macro or typedef.  */

#ifdef __OBJC2__
/* Const String Class ref.  */
typedef Class TNS_STRING_REF_T;
#else
/* Const String Class ref.  */
/* We need objc_class - but we don't need endless reminders that it's deprecated.  */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
typedef struct objc_class TNS_STRING_REF_T;
#pragma GCC diagnostic pop
#endif

#endif  /*__NEXT_RUNTIME__ */
#endif /* _OBJC_TEST_SUITE_TYPES_H_ */
