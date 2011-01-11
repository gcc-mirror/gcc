/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */

/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
/* { dg-options "-fconstant-string-class=MyTestString" } */
/* { dg-options "-mno-constant-cfstrings -fconstant-string-class=MyTestString" { target *-*-darwin* } } */

#include "../../objc-obj-c++-shared/objc-test-suite-types.h"

#include <stdlib.h> /* For abort() */

@interface MyTestString
{
  void *dummy_class_ptr;
  char *string;
  unsigned int len;
}
+ initialize;
/* All strings should contain the C string 'test'.  Call -check to
   test that this is true.  */
- (void) check;
@end

@implementation MyTestString
+ initialize {return self;}

- (void) check
{
  if (len != 4 || string[0] != 't' || string[1] != 'e'
      || string[2] != 's' || string[3] != 't' || string[4] != '\0')
    abort ();
}
@end

TNS_STRING_REF_T _MyTestStringClassReference; /* Only used by NeXT.  */

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

#ifdef __NEXT_RUNTIME__
/* The MyTestString metaclass will need to be initialized before we can
   send messages to strings.  */
#include <string.h>

void testsuite_mytest_string_init (void) __attribute__((constructor));
void testsuite_mytest_string_init (void) {
  memcpy (&_MyTestStringClassReference,
	  objc_getClass ("MyTestString"),
	  sizeof (_MyTestStringClassReference));
}
#endif