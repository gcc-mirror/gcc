/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, February 2011.  */
/* Ensure that -Wpadded generates no warnings during runtime structure metadata
   generation.  */
/* { dg-do compile } */
/* { dg-options "-Wpadded" } */

#include "../objc-obj-c++-shared/Object1.h"

/* Implement a class, so that the metadata generation happens.  */
@interface MyClass : Object
@end

@implementation MyClass
@end
