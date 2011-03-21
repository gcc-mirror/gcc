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
 * Compatibility header.
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
#ifndef _OBJC_OBJECT1_H_
#define _OBJC_OBJECT1_H_

#ifndef __NEXT_RUNTIME__ 
/* Case 1 = GNU. */
#  include <objc/Object.h>
/* NeXT requires a +initialize (or forward:) method, and it makes testcases more
   readable if the conditional code can be reduced, so we add one to the GNU tests 
   too.  This saves us from having to introduce it every time.  */
@interface Object (TEST_SUITE_ADDITIONS)
+ initialize;
@end

#else /* NeXT */

#  include "next-abi.h"
#  if !defined(NEXT_OBJC_ABI_VERSION) || (NEXT_OBJC_ABI_VERSION < 2)
/* Cases 2, Case 3/m32 and 4/m32 are handled as default.  */
#    include <objc/Object.h>
#  else
#    include <objc/objc.h>

/* This is a cut-down Object with only the methods currently required
   by the testsuite declared.  The implementation is provided in 
   Object1-implementation.h
*/

/* The m64 libobjc implementation of Object provides only the 'class' and
   isEqual: methods.  
   
   We add the others required as a test-suite category. 
   
   Please leave the unimplemented methods as comments - so that they can
   be inserted as required by future tests.  */

@interface Object
{
  Class isa;
}
+ (Class) class;
- (BOOL)isEqual: (id)anObject;
@end

/* Dummy definition.  */
typedef void * marg_list;

@interface Object (TEST_SUITE_ADDITIONS)

+ initialize;
- init;

+ new;
+ free;
- free;
+ alloc;
//- copy;
//+ allocFromZone:(void *)zone;
//- copyFromZone:(void *)zone;
//- (void *)zone;

- (Class) class;
+ (Class) superclass;
//+ (const char *) name;
//- superclass;
- (const char *) name;

//- self;
//- (unsigned int) hash;

/* Testing inheritance relationships */

//- (BOOL) isKindOf: aClassObject;
//- (BOOL) isMemberOf: aClassObject;
//- (BOOL) isKindOfClassNamed: (const char *)aClassName;
//- (BOOL) isMemberOfClassNamed: (const char *)aClassName;

/* Testing class functionality */

//+ (BOOL) instancesRespondTo:(SEL)aSelector;
//- (BOOL) respondsTo:(SEL)aSelector;

/* Testing protocol conformance */

- (BOOL) conformsTo: (Protocol *)aProtocolObject;
//+ (BOOL) conformsTo: (Protocol *)aProtocolObject;

/* Obtaining method descriptors from protocols */

//- (struct objc_method_description *) descriptionForMethod:(SEL)aSel;
//+ (struct objc_method_description *) descriptionForInstanceMethod:(SEL)aSel;

/* Obtaining method handles */

//- (IMP) methodFor:(SEL)aSelector;
//+ (IMP) instanceMethodFor:(SEL)aSelector;

/* Sending messages determined at run time */

//- perform:(SEL)aSelector;
//- perform:(SEL)aSelector with:anObject;
//- perform:(SEL)aSelector with:object1 with:object2;

/* Posing */

//+ poseAs: aClassObject;

/* Enforcing intentions */
 
//- subclassResponsibility:(SEL)aSelector;
//- notImplemented:(SEL)aSelector;

/* Error handling */

//- doesNotRecognize:(SEL)aSelector;
//- error:(const char *)aString, ...;

/* Debugging */

//- (void) printForDebugger:(void *)stream;

/* Archiving */

//- awake;
//- write:(void *)stream;
//- read:(void *)stream;
//+ (int) version;
//+ setVersion: (int) aVersion;

/* Forwarding */

- forward: (SEL)sel : (marg_list)args;
//- performv: (SEL)sel : (marg_list)args;

@end

#    endif /* NeXT case 3 & 4 m64 */
#  endif /* NEXT */
#endif /* _OBJC_OBJECT1_H_ */
