/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do compile } */

/* Test that @synthesize does not ICE if asked to use a non-existing
   ivar.  */

#include <objc/objc.h>

@interface Test
@property int v1;
@end

@implementation Test
@synthesize v1;       /* { dg-error "must be an existing ivar" } */
@end
/* { dg-warning "incomplete implementation" "" { target *-*-* } 15 } */
/* { dg-warning "method definition for .-setV1:. not found" "" { target *-*-* } 15 } */
/* { dg-warning "method definition for .-v1. not found" "" { target *-*-* } 15 } */
