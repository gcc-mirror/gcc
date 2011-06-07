/* Very simple root class for writing testcases.
   Copyright (C) 2011 Free Software Foundation, Inc.
   Contributed by Nicola Pero

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

#ifndef _TESTSUITE_OBJECT_H_
#define _TESTSUITE_OBJECT_H_

/* We use this root class instead of Object to keep the tests
   independent of the runtime being used.  Keep it simple.  */

@interface TestsuiteObject
{
  Class isa;
}
/* Required by the NeXT runtime.  Does nothing.  */
+ (id) initialize;

/* Creating instances.  */
+ (id) new;
+ (id) alloc;
- (id) init;
- (id) free;

/* Auxiliary methods.  */
+ (Class) class;
+ (Class) superclass;
+ (const char *)name;
- (const char *)name;
@end

#endif /* _TESTSUITE_OBJECT_H_ */
