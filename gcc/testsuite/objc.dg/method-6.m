/* Check that sending messages to variables of type 'Class' does not involve instance methods.  */
/* Author: Ziemowit Laski <zlaski@apple.com>  */
/* { dg-do compile } */

#include <objc/Protocol.h>

@interface Base
- (unsigned)port;
- (id)starboard;
@end

@interface Derived: Base
- (Object *)port;
+ (Protocol *)port;
@end

id foo(void) {
  Class receiver;
  id p = [receiver port];  /* there should be no warnings here! */
  p = [receiver starboard];  /* { dg-warning ".Class. may not respond to .\\+starboard." } */
       /* { dg-warning "Messages without a matching method signature" "" { target *-*-* } 20 } */
       /* { dg-warning "will be assumed to return .id. and accept" "" { target *-*-* } 20 } */
       /* { dg-warning ".\.\.\.. as arguments" "" { target *-*-* } 20 } */
  p = [Class port];  /* { dg-error ".Class. is not an Objective\\-C class name or alias" } */
  return p;
}
