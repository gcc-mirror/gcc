/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do run } */
/* { dg-options "-fobjc-exceptions" } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

/* This test checks the syntax @catch (...) which catches any
   exceptions.  Check that code using it runs correctly.  */

#include "../objc-obj-c++-shared/TestsuiteObject.m"
#include <stdlib.h>

@interface MyObject : TestsuiteObject
@end

@implementation MyObject
@end

int test (id object)
{
  int i = 0;

  @try
    {
      @throw object;
    }
  @catch (MyObject *o)
    {
      i += 1;
    }
  @catch (...)
    {
      i += 2;
    }
  @finally
    {
      i += 4;
    }

  return i;
}

int main (void)
{
  if (test ([MyObject new]) != 5)
    abort ();

  if (test ([TestsuiteObject new]) != 6)
    abort ();

  return 0;
}
