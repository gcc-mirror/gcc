/* Test if ObjC types play nice in conditional expressions.  */
/* Author: Ziemowit Laski  */

/* { dg-do compile } */
/* { dg-options "-fconstant-string-class=Foo" } */
/* { dg-options "-mno-constant-cfstrings -fconstant-string-class=Foo" { target *-*-darwin* } } */

#ifdef __NEXT_RUNTIME__
# include "../../objc-obj-c++-shared/F-NSObject.h"
# define OBJECT NSObject
#else
# include <objc/Object.h>
# define OBJECT Object
#endif
#include "../../objc-obj-c++-shared/objc-test-suite-types.h"

@interface Foo: OBJECT {
  char *cString;
  unsigned int len;
}
+ (id)description;
@end

@interface Bar: OBJECT
+ (Foo *) getString: (int) which;
@end

TNS_STRING_REF_T _FooClassReference;  /* Only used by NeXT.  */

@implementation Bar
+ (Foo *) getString: (int) which {
  return which? [Foo description]: @"Hello";
}
@end
