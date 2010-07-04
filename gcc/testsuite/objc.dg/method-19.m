/* The following should NOT generate "may not respond to" warnings, since a forward-declared
   @class (instance) should be treated like a 'Class') ('id').  */

/* { dg-do compile } */

#include "../objc-obj-c++-shared/Object1.h"

@class NotKnown;

void foo(NotKnown *n) {
  [NotKnown new];
  [n nonexistent_method]; /* { dg-warning "no .\\-nonexistent_method. method found" } */
}

/* { dg-warning "Messages without a matching method signature" "" { target *-*-* } 0 } */
/* { dg-warning "will be assumed to return .id. and accept" "" { target *-*-* } 0 } */
/* { dg-warning ".\.\.\.. as arguments" "" { target *-*-* } 0 } */
