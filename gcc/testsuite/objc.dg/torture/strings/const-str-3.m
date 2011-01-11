/* Test the -fconstant-string-class=Foo option under the NeXT runtime.  */
/* Developed by Markus Hitter <mah@jump-ing.de>.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
/* { dg-options "-fconstant-string-class=Foo" } */
/* { dg-options "-mno-constant-cfstrings -fconstant-string-class=Foo" { target *-*-darwin* } } */

#include "../../../objc-obj-c++-shared/objc-test-suite-types.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

@interface Foo {
  void *dummy_class_ref;
  char *cString;
  unsigned int len;
}
+ initialize;
- (char *)customString;
@end

TNS_STRING_REF_T _FooClassReference; /* Only used by NeXT.  */

@implementation Foo
+ initialize {return self;}

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

#ifdef __NEXT_RUNTIME__
  /* This memcpy has to be done before the first message is sent to a
     constant string object. Can't be moved to +initialize since _that_
     is already a message.  */

  memcpy(&_FooClassReference, objc_getClass("Foo"), sizeof(_FooClassReference));
#endif
  if (strcmp ([string customString], "bla")) {
    abort ();
  }

  printf([@"This is a working constant string object\n" customString]);
  return 0;
}
