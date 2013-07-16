/* Graceful handling of a syntax error.  */
/* { dg-do compile } */

#ifdef __NEXT_RUNTIME__
#include <Foundation/NSObject.h>
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
  NXLog("Hello, world!"); /* { dg-error "expected .\{. before .NXLog." } */
} /* { dg-error "stray .\}. between Objective\\-C\\+\\+ methods" } */
@end

/* { dg-error "expected constructor, destructor, or type conversion before" "" { target *-*-* } 28 } */
