/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, October 2010.  */
/* { dg-do compile } */
/* { dg-options "-Wall" } */
/* { dg-additional-options "-Wno-objc-root-class" } */

#include <objc/objc.h>
#include <stdlib.h>
/* Ensure a compatible definition of nil.  */
#include "../../objc-obj-c++-shared/objc-test-suite-types.h"

@interface NSArray
{
  Class isa;
} 
+ (id) arrayWithObject: (id)object __attribute__ ((sentinel));            /* { dg-warning "attribute only applies to variadic functions" } */
+ (id) arrayWithObjects: (id)firstObject, ... __attribute__ ((sentinel));

- (id) initWithObject: (id)object __attribute__ ((sentinel));            /* { dg-warning "attribute only applies to variadic functions" } */
- (id) initWithObjects: (id)firstObject, ... __attribute__ ((sentinel));
@end

void test (id object)
{
  NSArray *array;

  array = [NSArray arrayWithObject: object];
  array = [NSArray arrayWithObjects: object, nil];
  array = [NSArray arrayWithObjects: object, object, nil];
  array = [NSArray arrayWithObjects: object];               /* { dg-warning "not enough variable arguments" } */
  array = [NSArray arrayWithObjects: object, object];       /* { dg-warning "missing sentinel" } */

  [array initWithObject: object];
  [array initWithObjects: object, nil];
  [array initWithObjects: object, object, nil];
  [array initWithObjects: object];               /* { dg-warning "not enough variable arguments" } */
  [array initWithObjects: object, object];       /* { dg-warning "missing sentinel" } */
}
