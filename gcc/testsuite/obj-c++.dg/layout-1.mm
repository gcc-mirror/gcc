/* Ensure that we do not get bizarre warnings referring to
   __attribute__((packed)) or some such.  */
/* { dg-do compile } */
/* { dg-options "-Wpadded -Wpacked -Wabi" } */

#include <objc/Object.h>

@interface Derived1: Object
{ }
@end

@interface Derived2: Object
- (id) foo;
@end

/* { dg-excess-errors "In file included from" { target lp64 } } */
/* { dg-bogus "padding struct to align" "PR23610" { xfail lp64 } 1 } */
