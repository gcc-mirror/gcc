/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-options "-fobjc-exceptions" } */
/* { dg-do compile } */
// { dg-additional-options "-Wno-objc-root-class" }

/* This test checks the syntax @catch (...) which catches any
   exceptions.  At the moment, @catch (...) is identical to @catch (id
   exception).  */

#include <objc/objc.h>

@interface MyObject
{
  Class isa;
}
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
