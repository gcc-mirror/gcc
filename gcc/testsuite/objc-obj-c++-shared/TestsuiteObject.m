/* Very simple root class for writing testcases.
   Copyright (C) 2011-2025 Free Software Foundation, Inc.
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

/* This is the implementation, but in all simple testcases we
   recommend simply including it in the testcase.  */

#include "TestsuiteObject.h"
#include "runtime.h"

@implementation TestsuiteObject
+ (id) initialize
{
  return self;
}
+ (id) new
{
  return [[self alloc] init];
}
+ (id) alloc
{
  return class_createInstance (self, 0);
}
- (id) init
{
  return self;
}
/* We return 'id' to have the same signature as [Object -free] in
   older runtimes and avoid warnings about conflicting signatures.  */
- (id) free
{
  /* Cast 'self' to 'id' because the NeXT runtime in darwin8 (Apple
     Mac OS X 10.4) declares object_dispose to take an "Object *"
     argument.  */
  return object_dispose ((id)self);
}
+ (Class) class
{
  return self;
}
+ (Class) superclass
{
  return class_getSuperclass (self);
}
+ (const char *)name
{
  return class_getName (self);
}
- (const char *)name
{
  return object_getClassName (self);
}

- (id) retain
{
  return self;
}

- (void) release
{
  return;
}

@end
