/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-options "-fobjc-exceptions" } */
/* { dg-do compile } */

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
    {          /* { dg-error "expected" } */
      dummy++; /* { dg-error "'@catch' parameter is not a known Objective-C class type" "" { target *-*-* } .-1 } */
    }
  @catch ()  /* { dg-error "expected identifier before" } */
    {        /* { dg-error "'@catch' parameter is not a known Objective-C class type" "" { target *-*-* } .-1 } */
      dummy++;
    }
  @catch (i) /* { dg-error ".i. has not been declared" } */
    {        /* { dg-error "'@catch' parameter is not a known Objective-C class type" "" { target *-*-* } .-1 } */
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
  @catch MyObject2 *x)     /* { dg-error "expected ... before .MyObject2." } */
   {
     dummy++;
   }

  @try { @throw object; }
  @catch (MyObject *x)
  @catch (MyObject2 *y)    /* { dg-error "expected ... before .catch." } */

  return dummy;            /* { dg-error "expected ... before .return." } */
}
