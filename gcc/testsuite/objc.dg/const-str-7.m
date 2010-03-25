/* Test to make sure that the const objc strings are the same across scopes. */
/* Developed by Andrew Pinski <pinskia@physics.uc.edu> */
/* { dg-options "-fconstant-string-class=Foo " } */
/* { dg-do run } */

#include "../objc-obj-c++-shared/Object1.h"
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>

@interface Foo: Object {
  char *cString;
  unsigned int len;
}
- (char *)customString;
@end

#ifndef NEXT_OBJC_USE_NEW_INTERFACE
struct objc_class _FooClassReference;
#else
Class _FooClassReference;
#endif

@implementation Foo : Object
- (char *)customString {
  return cString;
}
@end

int main () {
  Foo *string = @"bla";
  {
    Foo *string2 = @"bla";

    if(string != string2)
      abort();
    printf("Strings are being uniqued properly\n");
   }
  return 0;
}

#include "../objc-obj-c++-shared/Object1-implementation.h"
