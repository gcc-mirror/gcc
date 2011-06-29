/* Test for spurious "may or may not return a value" warnings.  */
/* { dg-do compile } */
/* { dg-options "-Wreturn-type -Wextra" } */

#include "../objc-obj-c++-shared/TestsuiteObject.h"

@interface Foo: TestsuiteObject
- (id) meth1;
- (void) meth2;
@end

extern int bar;

@implementation Foo
- (id) meth1 {
  if (bar)
    return [TestsuiteObject new];
  return; /* { dg-error "return.statement with no value" } */
}
- (void) meth2 {
  if (!bar)
    return;
  bar = 0;
}
@end
