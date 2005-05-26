// Check if ObjC classes with non-POD C++ ivars are specially marked in the metadata.

// { dg-do run { target *-*-darwin* } }
// { dg-options "-fobjc-call-cxx-cdtors -fnext-runtime" }

#include <objc/objc-runtime.h>
#include <stdlib.h>
#define CHECK_IF(expr) if(!(expr)) abort()

#ifndef CLS_HAS_CXX_STRUCTORS
#define CLS_HAS_CXX_STRUCTORS 0x2000L
#endif

struct cxx_struct {
  int a, b;
  cxx_struct (void) { a = b = 55; }
};

@interface Foo {
  int c;
  cxx_struct s;
}
@end

@interface Bar: Foo {
  float f;
}
@end

@implementation Foo
@end

@implementation Bar
@end

int main (void)
{
  Class cls;

  cls = objc_getClass("Foo");
  CHECK_IF(cls->info & CLS_HAS_CXX_STRUCTORS);
  cls = objc_getClass("Bar");
  CHECK_IF(!(cls->info & CLS_HAS_CXX_STRUCTORS));

  return 0;
}
