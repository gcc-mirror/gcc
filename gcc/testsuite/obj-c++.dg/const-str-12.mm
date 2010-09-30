/* Test if ObjC types play nice in conditional expressions.  */
/* Author: Ziemowit Laski  */

/* { dg-options "-fconstant-string-class=Foo" } */
/* { dg-do compile } */

#include "../objc-obj-c++-shared/Object1.h"
#import "../objc-obj-c++-shared/next-mapping.h"

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
struct fudge_objc_class _FooClassReference;
#else
struct objc_class _FooClassReference;
#endif

@implementation Bar
+ (Foo *) getString: (int) which {
  return which? [Foo description]: @"Hello";
}
@end
