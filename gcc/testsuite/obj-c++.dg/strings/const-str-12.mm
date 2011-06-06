/* Test if ObjC types play nice in conditional expressions.  */
/* Author: Ziemowit Laski  */

/* { dg-do compile } */
/* { dg-options "-fconstant-string-class=Foo" } */
/* { dg-options "-mno-constant-cfstrings -fconstant-string-class=Foo" { target *-*-darwin* } } */

#include <objc/Object.h>
#include "../../objc-obj-c++-shared/objc-test-suite-types.h"

@interface Foo: Object {
  char *cString;
  unsigned int len;
}
+ (id)description;
@end

@interface Bar: Object
+ (Foo *) getString: (int) which;
@end

TNS_STRING_REF_T _FooClassReference;  /* Only used by NeXT.  */

@implementation Bar
+ (Foo *) getString: (int) which {
  return which? [Foo description]: @"Hello";
}
@end
