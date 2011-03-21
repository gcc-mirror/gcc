/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, January 2011.  */
/* { dg-do compile } */

#include <objc/objc.h>

@interface MyRootClass
{
  Class isa;
}
/* Test missing '=' in setter/getter attributes.  */
@property (getter)  int property_a; /* { dg-error "missing .=. .after .getter. attribute." } */
@property (setter) int property_b;  /* { dg-error "missing .=. .after .setter. attribute." } */
@property (assign, getter) int property_c; /* { dg-error "missing .=. .after .getter. attribute." } */
@end
