/* Test to make sure that the const objc strings are the same across
   scopes.  */
/* Developed by Andrew Pinski <pinskia@physics.uc.edu> */


/* { dg-options "-fnext-runtime -fconstant-string-class=Foo -lobjc" } */
/* { dg-do run { target *-*-darwin* } } */


#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <objc/objc.h>
#include <objc/Object.h>


@interface Foo: Object {
  char *cString;
  unsigned int len;
}
- (char *)customString;
@end

struct objc_class _FooClassReference;


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

