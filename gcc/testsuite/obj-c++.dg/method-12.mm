/* Check that sending messages to variables of type 'Class' does not involve instance methods, unless they reside in root classes.  */
/* Author: Ziemowit Laski <zlaski@apple.com>  */
/* { dg-options "-Wstrict-selector-match" } */
/* { dg-do compile } */
/* { dg-skip-if "Object interface removed" { *-*-darwin[1-2]* && { lp64 } } { "-fnext-runtime" } { "" } } */

#include <objc/Protocol.h>

@interface Base
- (unsigned)port; /* { dg-line Base_port } */
@end

@interface Derived: Base
- (Object *)port;
+ (Protocol *)port; /* { dg-line Derived_port_last } */
- (id)starboard;
@end

void foo(void) {
  Class receiver;

  [receiver port];  /* { dg-warning "multiple methods named .\\+port. found" } */
       /* { dg-message "using .\\-\\(unsigned( int)?\\)port." "" { target *-*-* } Base_port } */
       /* { dg-message "also found .\\+\\(Protocol \\*\\)port." "" { target *-*-* } Derived_port_last } */

  [receiver starboard];  /* { dg-warning "no .\\+starboard. method found" } */
       /* { dg-warning "Messages without a matching method signature" "" { target *-*-* } .-1 } */
       /* { dg-warning "will be assumed to return .id. and accept" "" { target *-*-* } .-2 } */
       /* { dg-warning ".\.\.\.. as arguments" "" { target *-*-* } .-3 } */

  [Class port];  /* { dg-error ".Class. is not an Objective\\-C class name or alias" } */
}
