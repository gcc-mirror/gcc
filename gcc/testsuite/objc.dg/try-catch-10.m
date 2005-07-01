/* Ensure that @try/@catch blocks do not mess with types of
   local objects (other than their volatile bits).  */

/* { dg-options "-fobjc-exceptions -fnext-runtime" } */
/* { dg-do compile } */

#include <objc/Object.h>

@protocol Proto1
- (int)meth1;
@end

@protocol Proto2
- (int)meth2;
@end

@interface MyClass: Object <Proto2> {
  int a;
}
- (int)meth2;
- (Object *)parm1: (id)p1 parm2: (id<Proto1>)p2;
@end

MyClass *mc1, *mc2;

@implementation MyClass
- (int)meth2 {
  return a;
}
- (Object *)parm1: (id)p1 parm2: (id<Proto1>)p2 {
  @try {
    mc2 = p2;   /* { dg-warning "type .id <Proto1>. does not conform to the .Proto2. protocol" } */
  }
  @catch (id exc) {
    return exc;
  }
  mc1 = p1;  /* no warning here! */
  return self;
}
@end  
