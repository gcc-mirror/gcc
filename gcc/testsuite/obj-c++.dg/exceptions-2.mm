/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */

/* FIXME: This does not test running the code, because Objective-C exceptions at the moment
   do not execute correctly in Objective-C++.  See PR objc++/23616.  Once that is fixed,
   this test should be changed to use 'dg-do run' instead of just 'dg-do compile'.  */
/* { dg-do compile } */
/* { dg-options "-fobjc-exceptions" } */

/* This test checks the syntax @catch (...) which catches any
   exceptions.  Check that code using it runs correctly.  */

#include "../objc-obj-c++-shared/Object1.h"
#include <stdlib.h>

@interface MyObject : Object
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

  if (test ([Object new]) != 6)
    abort ();

  return 0;
}
