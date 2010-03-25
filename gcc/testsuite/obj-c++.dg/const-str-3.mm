/* Test the -fconstant-string-class=Foo option under the NeXT
   runtime.  */
/* Developed by Markus Hitter <mah@jump-ing.de>.  */

/* { dg-options "-fconstant-string-class=Foo" } */
/* { dg-do run { target *-*-darwin* } } */

#import "../objc-obj-c++-shared/Object1.h"
#import "../objc-obj-c++-shared/next-mapping.h"
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <objc/objc.h>
#ifndef __NEXT_RUNTIME__
#include <objc/objc-api.h>
#endif

@interface Foo: Object {
  char *cString;
  unsigned int len;
}
- (char *)customString;
@end

#ifdef NEXT_OBJC_USE_NEW_INTERFACE
struct fudge_objc_class _FooClassReference;
#else
struct objc_class _FooClassReference;
#endif

@implementation Foo : Object
- (char *)customString {
  return cString;
}
@end

int main () {
  Foo *string = @"bla";
  Foo *string2 = @"bla";

  if(string != string2)
    abort();
  printf("Strings are being uniqued properly\n");

  /* This memcpy has to be done before the first message is sent to a
     constant string object. Can't be moved to +initialize since _that_
     is already a message. */

  memcpy(&_FooClassReference, objc_get_class("Foo"), sizeof(_FooClassReference));
  if (strcmp ([string customString], "bla")) {
    abort ();
  }

  printf([@"This is a working constant string object\n" customString]);
  return 0;
}
#include "../objc-obj-c++-shared/Object1-implementation.h"
