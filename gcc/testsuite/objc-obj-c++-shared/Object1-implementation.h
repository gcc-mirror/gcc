/* Compatibility code between APIs and ABIs for the objc test suite.
   Copyright (C) 2010, 2011 Free Software Foundation, Inc.
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

/* 
 * Implementation of a compatibility layer for the ObjC* test-suite.
 *
 * Four cases:
 *   GNU 
 *      Uses the 'old' Object with API and ABI = 0.
 *      Compatibility methods are added.
 *   NeXT pre-Darwin9
 *      Uses the 'old' Object with API and ABI = 0.
 *   NeXT Darwin >= 9 with no implementation of ABI 2
 *      Uses API 2 and ABI 0 for m32, uses the 'old' Object'
 *      Uses API 2 for m64 but only compile tests can be expected to work.
 *   NeXT Darwin >= 9 with __OBJC2__
 *      Uses API 2 and ABI 0 for m32, uses the 'old' Object'
 *      Uses API 2 and ABI 2 - the libobjc implementation of Object is very
 *      basic, and we add a category to expand this for test-suite use.
 */

#ifndef _OBJC_OBJECT1_IMPLEMENTATION_H_
#define _OBJC_OBJECT1_IMPLEMENTATION_H_

#include "Object1.h"

#ifndef __NEXT_RUNTIME__

/* Save us from repeating this.  */
@implementation Object (TEST_SUITE_ADDITIONS)
+ initialize 
{
  return self;
}
@end

#else

/* For NeXT pre-Darwin 9 or m32 we need do nothing.  */

#  if NEXT_OBJC_ABI_VERSION >= 2 

/* Pick up the API=2 header.  */
#    include <objc/runtime.h>

#    ifndef __OBJC2__

/* On a Darwin system >= 9 when there is no __OBJC2__ compiler, the testcases
   will not link.  So we provide a dummy Object for this purpose.  */

@implementation Object

+ (Class) class 
{
  return self;
}

- (BOOL)isEqual: (id)anObject
{
  return self == anObject;
}

@end
#    endif  /* __OBJC2__ */

/* In any case, since the library does not provide a complete (enough) 
   implementation we need to provide the additions.  */

@implementation Object (TEST_SUITE_ADDITIONS)

+ initialize 
{
  return self;
}

- init 
{
  return self;
}

- (Class) class 
{
  return isa;
}

+ (Class) superclass
{
  return class_getSuperclass(object_getClass(self));
}

+ new 
{
  return [[self alloc] init];
}

+ free 
{
  return nil;
}

- free 
{
  return object_dispose(self);
}

+ alloc 
{
  return class_createInstance (self, 0);
}

- (Class) superclass {
  return class_getSuperclass([self class]);
}

- (const char *) name {
  return class_getName([self class]);
}

-(BOOL)conformsTo:(Protocol *)protocol {
  Class cls;
  for (cls = [self class]; cls; cls = [cls superclass]) 
    {
      if (class_conformsToProtocol(cls, protocol)) 
	return YES;
    }
  return NO;
}

#ifdef __cplusplus
extern "C" {
#endif
extern int printf (const char *, ...);
extern void abort (void);
#ifdef __cplusplus
}
#endif

/* This is a helper to catch cases where we need to add more functionality
   to our test-suite category - more informative than fail with 'does not 
   respond to forward:'  */
- forward: (SEL)sel : (marg_list)args
{
  const char * onam = object_getClassName (self);
  const char * snam = sel_getName (sel);
  printf ("%s: tried to forward: %s\n", onam, snam);
  abort ();
}
@end

#   endif /* NEXT_OBJC_ABI_VERSION >= 2  */
#  endif /* __NEXT_RUNTIME__ */
#endif /* _OBJC_OBJECT1_IMPLEMENTATION_H_ */
