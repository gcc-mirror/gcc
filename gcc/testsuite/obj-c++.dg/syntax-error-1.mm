/* Graceful handling of a syntax error.  */
/* { dg-do compile } */
/* Suppress warnings that the GNUStep headers introduce.  */
/* { dg-additional-options "-std=gnu++11 -Wno-expansion-to-defined -Wno-variadic-macros" { target *-*-darwin* } } */

#ifdef __NEXT_RUNTIME__
#include "../objc-obj-c++-shared/F-NSObject.h"
#define OBJECT NSObject
#else
#include <objc/Object.h>
#define OBJECT Object
#endif

class foo {
  public:
    foo();
    virtual ~foo();
};


extern void NXLog(const char *, ...);

@interface Test2 : OBJECT {
}
- (void) foo2;
@end

@implementation Test2
- (void) foo2
  NXLog("Hello, world!"); /* { dg-line Test2_foo2_body } */
  /* { dg-error "expected .\{. before .NXLog." "" { target *-*-* } Test2_foo2_body } */
} /* { dg-error "stray .\}. between Objective\\-C\\+\\+ methods" } */
@end

/* { dg-error "expected constructor, destructor, or type conversion before" "" { target *-*-* } Test2_foo2_body } */
