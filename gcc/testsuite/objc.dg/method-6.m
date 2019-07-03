/* Check that sending messages to variables of type 'Class' does not involve instance methods,
   unless they reside in root classes.  */
/* Author: Ziemowit Laski <zlaski@apple.com>  */
/* { dg-do compile } */
/* { dg-options "-Wstrict-selector-match" } */

#ifdef __NEXT_RUNTIME__
# include "../objc-obj-c++-shared/F-NSObject.h"
# define OBJECT NSObject
#else
# include <objc/Object.h>
# include <objc/Protocol.h>
# define OBJECT Object
#endif

@interface Base
- (unsigned)port; /* { dg-line Base_port } */
@end

@interface Derived: Base
- (OBJECT *)port;
+ (Protocol *)port; /* { dg-line Derived_port_last } */
- (id)starboard;
@end

void foo(void) {
  Class receiver;

  [receiver port];  /* { dg-warning "multiple methods named .\\+port. found" } */
       /* { dg-message "using .\\-\\(unsigned( int)?\\)port." "" { target *-*-* } Base_port } */
       /* { dg-message "also found .\\+\\(Protocol \\*\\)port." "" { target *-*-* } Derived_port_last } */

  [receiver starboard];  /* { dg-warning "no .\\+starboard. method found" } */
  /* { dg-warning "messages without a matching method signature will be assumed to return .id. and accept .\.\.\.. as arguments" "" { target *-*-* } 0 } */

  [Class port];  /* { dg-error ".Class. is not an Objective\\-C class name or alias" } */
}
