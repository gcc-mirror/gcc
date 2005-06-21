/* Test the -fconstant-string-class=Foo option under the NeXT
   runtime.  */
/* Developed by Markus Hitter <mah@jump-ing.de>.  */

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
  Foo *string2 = @"bla";

  if(string != string2)
    abort();
  printf("Strings are being uniqued properly\n");

  /* This memcpy has to be done before the first message is sent to a
     constant string object. Can't be moved to +initialize since _that_
     is already a message. */

  memcpy(&_FooClassReference, objc_getClass("Foo"), sizeof(_FooClassReference));
  if (strcmp ([string customString], "bla")) {
    abort ();
  }

  printf([@"This is a working constant string object\n" customString]);
  return 0;
}
