/* Test if ObjC types play nice in conditional expressions.  */
/* Author: Ziemowit Laski  */

/* { dg-do compile } */
/* { dg-options "-fconstant-string-class=Foo" } */
/* { dg-options "-mno-constant-cfstrings -fconstant-string-class=Foo" { target *-*-darwin* } } */

#include "../../objc-obj-c++-shared/Object1.h"

@interface Foo: Object {
  char *cString;
  unsigned int len;
}
+ (id)description;
@end

@interface Bar: Object
+ (Foo *) getString: (int) which;
@end

#ifdef NEXT_OBJC_USE_NEW_INTERFACE
Class _FooClassReference;
#else
struct objc_class _FooClassReference;
#endif

@implementation Bar
+ (Foo *) getString: (int) which {
  return which? [Foo description]: @"Hello";
}
@end
