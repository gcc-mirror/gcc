/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, January 2011.  */
/* { dg-do compile } */

#include <objc/objc.h>

@interface MyRootClass
{
  Class isa;
}
/* Test missing '=' in setter/getter attributes.  */
@property (getter)  int property_a; /* { dg-error {expected '=' after Objective-C 'getter'} } */
@property (setter) int property_b;  /* { dg-error {expected '=' after Objective-C 'setter'} } */
@property (assign, getter) int property_c; /* { dg-error {expected '=' after Objective-C 'getter'} } */
@property (retain, getter=) id x; /* { dg-error {expected 'getter' selector name} } */
@property (retain, setter=) id y; /* { dg-error {expected 'setter' selector name} } */
@end
