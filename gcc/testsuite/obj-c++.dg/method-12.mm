/* Check that sending messages to variables of type 'Class' does not involve instance methods, unless they reside in root classes.  */
/* Author: Ziemowit Laski <zlaski@apple.com>  */
/* { dg-options "-Wstrict-selector-match" } */
/* { dg-do compile } */
/* { dg-skip-if "Object interface removed" { *-*-darwin[1-2]* && { lp64 } } { "-fnext-runtime" } { "" } } */

#include <objc/Protocol.h>

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
       /* { dg-message "using .\\-\\(unsigned( int)?\\)port." "" { target *-*-* } 10 } */
       /* { dg-message "also found .\\+\\(Protocol \\*\\)port." "" { target *-*-* } 15 } */

  [receiver starboard];  /* { dg-warning "no .\\+starboard. method found" } */
       /* { dg-warning "Messages without a matching method signature" "" { target *-*-* } 26 } */
       /* { dg-warning "will be assumed to return .id. and accept" "" { target *-*-* } 26 } */
       /* { dg-warning ".\.\.\.. as arguments" "" { target *-*-* } 26 } */

  [Class port];  /* { dg-error ".Class. is not an Objective\\-C class name or alias" } */
}
