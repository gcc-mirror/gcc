/* Test for graceful compilation of @synchronized statements.  */

/* { dg-do compile } */
/* { dg-options "-fobjc-exceptions" } */

#include "../objc-obj-c++-shared/Object1.h"

@interface Derived: Object
- (id) meth;
@end

@implementation Derived
- (id) meth {
  return self;
}

static Derived* rewriteDict(void) {
  static Derived *sDict = 0;
  if (sDict == 0) {
    @synchronized ([Derived class]) {
      if (sDict == 0)
	sDict = [Derived new];
    }
  } 
  return sDict;
}
@end
