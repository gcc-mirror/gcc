/* Ensure that we do not get bizarre warnings referring to
   __attribute__((packed)) or some such.  */
/* { dg-do compile } */
/* { dg-options "-Wpadded -Wpacked -Wabi=8" } */

#include "../objc-obj-c++-shared/TestsuiteObject.h"

@interface Derived1: TestsuiteObject
{ }
@end

@interface Derived2: TestsuiteObject
- (id) foo;
@end

/* { dg-prune-output "In output included from" }   Ignore this message.  */
/* { dg-bogus "padding struct to align" "PR23610" { target *-*-* } 0 } */
