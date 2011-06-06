/* Test to make sure that the const objc strings are the same across scopes. */
/* Developed by Andrew Pinski <pinskia@physics.uc.edu> */
/* { dg-do run } */
/* { dg-options "-fconstant-string-class=Foo " } */
/* { dg-options "-mno-constant-cfstrings -fconstant-string-class=Foo" { target *-*-darwin* } } */

#include "../../../objc-obj-c++-shared/TestsuiteObject.m"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

@interface Foo: TestsuiteObject {
  char *cString;
  unsigned int len;
}
- (char *)customString;
@end

#ifdef __NEXT_RUNTIME__
#ifdef NEXT_OBJC_USE_NEW_INTERFACE
Class _FooClassReference;
#else
struct objc_class _FooClassReference;
#endif
#endif

@implementation Foo : TestsuiteObject
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
