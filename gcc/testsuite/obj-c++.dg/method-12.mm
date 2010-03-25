/* Check that sending messages to variables of type 'Class' does not involve instance methods, unless they reside in root classes.  */
/* Author: Ziemowit Laski <zlaski@apple.com>  */
/* { dg-options "-Wstrict-selector-match" } */
/* { dg-do compile } */

#include "../objc-obj-c++-shared/Protocol1.h"

@interface Base
- (unsigned)port;
@end

@interface Derived: Base
- (Object *)port;
+ (Protocol *)port;
- (id)starboard;
@end

void foo(void) {
  Class receiver;

  [receiver port];  /* { dg-warning "multiple methods named .\\+port. found" } */
       /* { dg-warning "using .\\-\\(unsigned( int)?\\)port." "" { target *-*-* } 9 } */
       /* { dg-warning "also found .\\+\\(Protocol \\*\\)port." "" { target *-*-* } 14 } */

  [receiver starboard];  /* { dg-warning "no .\\+starboard. method found" } */
       /* { dg-warning "Messages without a matching method signature" "" { target *-*-* } 25 } */
       /* { dg-warning "will be assumed to return .id. and accept" "" { target *-*-* } 25 } */
       /* { dg-warning ".\.\.\.. as arguments" "" { target *-*-* } 25 } */

  [Class port];  /* { dg-error ".Class. is not an Objective\\-C class name or alias" } */
}
