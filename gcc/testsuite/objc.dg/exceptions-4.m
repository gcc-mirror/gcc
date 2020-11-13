/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-options "-fobjc-exceptions" } */
/* { dg-do compile } */
/* { dg-additional-options "-Wno-objc-root-class" } */

/* Test warnings when parsing syntax errors in @catch().  */

#include <objc/objc.h>

@interface MyObject
{
  Class isa;
}
@end

@implementation MyObject
@end

@interface MyObject2
{
  Class isa;
}
@end

@implementation MyObject2
@end

@protocol MyProtocol;

int test (id object)
{
  int dummy = 0;

  @try { @throw object; }
  @catch
    { /* { dg-error "expected ... before ... token" } */
      dummy++;
    }
  @catch ()  /* { dg-error "expected declaration specifiers or ..... before ..." } */
    {
      dummy++;
    }
  @catch (i) /* { dg-error "unknown type name .i." } */
    {
      dummy++;
    }
  @catch (id <MyProtocol x) /* { dg-error "expected ... before .x." } */
    {                       /* { dg-error "'@catch' parameter cannot be protocol-qualified" "" { target *-*-* } .-1 } */
      dummy++;
    }
  @catch MyObject *x       /* { dg-error "expected ... before .MyObject." } */
    {
      dummy++;
    }
  @catch MyObject2 *x)      /* { dg-error "expected ... before .MyObject2." } */
   {
     dummy++;
   }

  @try { @throw object; }
  @catch (MyObject *x)
  @catch (MyObject2 *y)    /* { dg-error "expected ... before .catch." } */

  return dummy;
}
