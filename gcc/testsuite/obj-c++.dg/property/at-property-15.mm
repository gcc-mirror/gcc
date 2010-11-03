/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do compile } */
/* { dg-options "-Wno-property-assign-default" } */

#include <objc/objc.h>

/* Test that -Wno-property-assign-default turns off all "object
   property xxx has no assign, return or copy attribute" warnings.  */

@interface MyRootClass
{
  Class isa;
}

@property id property_a;            /* Would normally generate a warning.  */
@property (readonly) id property_b;
@property id *property_c;          
@property Class property_d;        
@property MyRootClass *property_e; /* Would normally generate a warning.  */
@end
