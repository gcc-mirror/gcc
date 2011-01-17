/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, January 2011.  */
/* { dg-options "-Wselector" } */
/* { dg-do compile } */

#include <objc/objc.h>

@interface RootObject
@end

@interface MyObject : RootObject
- (void) method; /* { dg-message "found" } */
@end

@interface MyObject2  : RootObject
- (int) method; /* { dg-message "also found" } */
@end /* { dg-warning "multiple selectors named .-method. found" } */
