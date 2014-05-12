/* Test instance variable scope.  */
/* Author: Dimitris Papavasiliou <dpapavas@gmail.com>.  */
/* { dg-do compile } */
/* { dg-additional-options "-fno-local-ivars" } */
#include <objc/objc.h>

@interface MyClass
{
  int someivar;
}
- (void) testscope;
- (void) testshadowing;
@end

@implementation MyClass
- (void) testscope
{
  int a;

  a = self->someivar;  /* No warning or error. */
  a = someivar;        /* { dg-error ".someivar. undeclared" } */
}

- (void) testshadowing
{
  int someivar = 1;
  int a;

  /* Since instance variables don't have local scope no shadowing
     should occur. */
  
  a = someivar; /* No warning. */
}
@end
