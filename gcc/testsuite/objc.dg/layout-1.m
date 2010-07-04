/* Ensure that we do not get bizarre warnings referring to
   __attribute__((packed)) or some such.  */
/* { dg-do compile } */
/* { dg-options "-Wpadded -Wpacked" } */

#include "../objc-obj-c++-shared/Object1.h"

@interface Derived1: Object
{ }
@end

@interface Derived2: Object
- (id) foo;
@end

