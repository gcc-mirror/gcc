/* Test instance variable scope.  */
/* Author: Dimitris Papavasiliou <dpapavas@gmail.com>.  */
/* { dg-do compile } */
#include <objc/objc.h>

@interface MyClass
{
  int someivar;
}
- (void) test;
@end

@implementation MyClass
- (void) test
{
  int a;

  /* Make sure instance variables do have local scope when
     -fno-local-ivar isn't specified. */
  
  a = self->someivar;  /* No warning or error. */
  a = someivar;        /* No error. */
}
@end
