/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do compile } */

/* Test that each @synthesize is using a different instance variable,
   and that it must belong to the class (not to a superclass).  */

#include <objc/objc.h>

@interface Test
{
  int v;
  int w;
}
@property int v1;
@property int v2;
@end
@implementation Test
@synthesize v1 = v;  /* { dg-message "originally specified here" } */
@synthesize v2 = v;  /* { dg-error "property .v2. is using the same instance variable as property .v1." } */
@end
@interface Test2 : Test
@property int w1;
@end

@implementation Test2
@synthesize w1;      /* { dg-error "ivar .w1. used by .@synthesize. declaration must be an existing ivar" } */
@end
/* { dg-warning "incomplete implementation" "" { target *-*-* } 27 } */
/* { dg-message "method definition for .-setW1:. not found" "" { target *-*-* } 27 } */
/* { dg-message "method definition for .-w1. not found" "" { target *-*-* } 27 } */
