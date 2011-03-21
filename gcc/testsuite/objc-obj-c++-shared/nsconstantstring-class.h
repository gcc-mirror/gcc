/* A small NSConstantString implementation for use with the NeXT runtime.
   Copyright (C) 2011 Free Software Foundation, Inc.

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

#ifndef _test_suite_nsconstantstring_class_h_
#define _test_suite_nsconstantstring_class_h_
#ifdef __NEXT_RUNTIME__

/* On full-fledged Mac OS X systems, NSConstantString is provided
   as part of the Foundation framework.  However, on bare Darwin systems,
   Foundation is not included, and hence there is no NSConstantString 
   implementation to link against.

   This code is derived from the GNU runtime's NXConstantString implementation.
*/

#include "objc-test-suite-types.h"

extern TNS_STRING_REF_T _NSConstantStringClassReference;

@interface NSConstantString
{
  Class isa;
  char *c_string;
  unsigned int len;
}

+ (id) initialize;

- (const char *) cString;
- (unsigned int) length;

@end

#endif /* __NEXT_RUNTIME__ */
#endif /* _test_suite_nsconstantstring_class_h_ */
