/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */

/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
/* { dg-options "-fconstant-string-class=MyTestString" } */
/* { dg-options "-mno-constant-cfstrings -fconstant-string-class=MyTestString" { target *-*-darwin* } } */

/* { dg-additional-sources "../objc-obj-c++-shared/Object1.m" } */

#include "../objc-obj-c++-shared/Object1.h"
#include "../objc-obj-c++-shared/next-mapping.h"

#include <stdlib.h> /* For abort() */

@interface MyTestString : Object
{
  char *string;
  unsigned int len;
}
/* All strings should contain the C string 'test'.  Call -check to
   test that this is true.  */
- (void) check;
@end

@implementation MyTestString
- (void) check
{
  if (len != 4 || string[0] != 't' || string[1] != 'e'
      || string[2] != 's' || string[3] != 't' || string[4] != '\0')
    abort ();
}
@end

int main (void)
{
  MyTestString *test_valid1 = @"test";
  MyTestString *test_valid2 = @"te" @"st";
  MyTestString *test_valid3 = @"te" @"s" @"t";
  MyTestString *test_valid4 = @ "t" @ "e" @ "s" @ "t";
  MyTestString *test_valid5 = @ "t" "e" "s" "t";
  MyTestString *test_valid6 = @ "t" "e" "s" @ "t";

  [test_valid1 check];
  [test_valid2 check];
  [test_valid3 check];
  [test_valid4 check];
  [test_valid5 check];
  [test_valid6 check];

  return 0;
}
