/* Graceful handling of a syntax error.  */
/* { dg-do compile } */

#include <objc/Object.h>

class foo {
  public:
    foo();
    virtual ~foo();
};


extern void NXLog(const char *, ...);

@interface Test2 : Object {
}
- (void) foo2;
@end

@implementation Test2
- (void) foo2
  NXLog("Hello, world!"); /* { dg-error "expected .\{. before .NXLog." } */
} /* { dg-error "stray .\}. between Objective\\-C\\+\\+ methods" } */
@end

/* { dg-error "expected constructor, destructor, or type conversion before" "" { target *-*-* } 22 } */
