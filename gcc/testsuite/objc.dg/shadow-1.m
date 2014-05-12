/* Test disabling of warnings for shadowing instance variables.  */
/* Author: Dimitris Papavasiliou <dpapavas@gmail.com>.  */
/* { dg-do compile } */
/* { dg-additional-options "-Wno-shadow-ivar" } */
#include <objc/objc.h>

@interface MyClass
{
@private
  int private;

@protected
  int protected;

@public
  int public;
}
- (void) test;
@end

@implementation MyClass
- (void) test
{
  int private = 12;
  int protected = 12;
  int public = 12;
  int a;
  
  a = private;    /* No warning. */
  a = protected;  /* No warning. */
  a = public;     /* No warning. */
}
@end
