/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, December 2010.  */
/* { dg-do compile } */
// { dg-additional-options "-Wno-objc-root-class" }

/* This test tests warnings on class extensions.  */

#include <objc/objc.h>

@interface MyObject
{
  Class isa;
  int count;
}
- (int) test;        /* { dg-message "previous declaration" } */
@property int count; /* { dg-message "originally specified here" } */
@end

@interface MyObject ()
- (void) test; /* { dg-error "duplicate declaration of method .-test." } */
@end

@interface MyObject ()
@end

@interface MyObject ()
@property int count; /* { dg-error "redeclaration of property .count." } */
@end
