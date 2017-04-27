/* Check that sending messages to variables of type 'Class' does not involve instance methods,
   unless they reside in root classes.  */
/* Author: Ziemowit Laski <zlaski@apple.com>  */
/* { dg-do compile } */
/* { dg-options "-Wstrict-selector-match" } */

#ifdef __NEXT_RUNTIME__
#include <Foundation/NSObject.h>
#define OBJECT NSObject
#else
#include <objc/Object.h>
#include <objc/Protocol.h>
#define OBJECT Object
#endif

@interface Base
- (unsigned)port;
@end

@interface Derived: Base
- (OBJECT *)port;
+ (Protocol *)port;
- (id)starboard;
@end

void foo(void) {
  Class receiver;

  [receiver port];  /* { dg-warning "multiple methods named .\\+port. found" } */
       /* { dg-message "using .\\-\\(unsigned( int)?\\)port." "" { target *-*-* } 17 } */
       /* { dg-message "also found .\\+\\(Protocol \\*\\)port." "" { target *-*-* } 22 } */

  [receiver starboard];  /* { dg-warning "no .\\+starboard. method found" } */
       /* { dg-warning "Messages without a matching method signature" "" { target *-*-* } .-1 } */
       /* { dg-warning "will be assumed to return .id. and accept" "" { target *-*-* } .-2 } */
       /* { dg-warning ".\.\.\.. as arguments" "" { target *-*-* } .-3 } */

  [Class port];  /* { dg-error ".Class. is not an Objective\\-C class name or alias" } */
}
