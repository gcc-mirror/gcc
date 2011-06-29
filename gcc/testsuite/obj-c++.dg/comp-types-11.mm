/* { dg-do compile } */

#include "../objc-obj-c++-shared/TestsuiteObject.h"

@protocol Foo
- (id)meth1;
- (id)meth2:(int)arg;
@end

@interface Derived1: TestsuiteObject
@end

@interface Derived2: TestsuiteObject
+ (Derived1 *)new;
@end

id<Foo> func(void) {
  TestsuiteObject *o = [TestsuiteObject new];
  return o;  /* { dg-warning "class .TestsuiteObject. does not implement the .Foo. protocol" } */
}

@implementation Derived2
+ (Derived1 *)new {
  Derived2 *o = [super new];
  return o;  /* { dg-warning "distinct Objective\\-C type in return" } */
}
@end
