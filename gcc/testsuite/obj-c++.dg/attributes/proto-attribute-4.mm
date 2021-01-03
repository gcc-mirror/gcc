/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, December 2010.  */
/* { dg-do compile } */
/* { dg-additional-options "-Wno-objc-root-class" } */

/* Test that you get a warning when an unknown protocol attribute is ignored.  */

#include <objc/objc.h>

__attribute__ ((unknown_attribute))
@protocol MyProtocol /* { dg-warning "ignored" } */
- (id) new;
@end

__attribute__ ((unknown_attribute))
@protocol MyProtocol2; /* { dg-warning "ignored" } */

/* Use the protocols to double-check that no more warnings are
   generated.  */

@interface MyClass <MyProtocol>
@end

int test (id <MyProtocol2> x)
{
  if (@protocol (MyProtocol) == @protocol (MyProtocol2))
    return 1;

  if (x)
    return 2;

  return 3;
}
