/* { dg-do compile } */

#include "../objc-obj-c++-shared/Object1.h"

@protocol Foo
- (id)meth1;
- (id)meth2:(int)arg;
@end

@interface Derived1: Object
@end

@interface Derived2: Object
+ (Derived1 *)new;
@end

id<Foo> func(void) {
  Object *o = [Object new];
  return o;  /* { dg-warning "class .Object. does not implement the .Foo. protocol" } */
}

@implementation Derived2
+ (Derived1 *)new {
  Derived2 *o = [super new];
  return o;  /* { dg-warning "distinct Objective\\-C type in return" } */
}
@end
