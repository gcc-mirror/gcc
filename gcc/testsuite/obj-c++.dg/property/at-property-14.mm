/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do compile } */

#include <objc/objc.h>

@interface MyRootClass
{
  Class isa;
}

/* Test the warnings on 'assign'.  */
@property id property_a;   /*  { dg-warning "object property .property.a. has no .assign., .retain. or .copy. attribute" } */
			   /*  { dg-message ".assign. can be unsafe for Objective-C objects" "" { target *-*-* } .-1 } */

@property (readonly) id property_b; /* No 'assign' warning (assign semantics do not matter if the property is readonly).  */
@property id *property_c;           /* No 'assign' warning (the type is not an Objective-C object).  */
@property Class property_d;         /* No 'assign' warning (Classes are static objects so assign semantics do not matter for them).  */
@property MyRootClass *property_e;  /* { dg-warning "object property .property.e. has no .assign., .retain. or .copy. attribute" } */
			            /* { dg-message ".assign. can be unsafe for Objective-C objects" "" { target *-*-* } .-1 } */
@end
