// Check if ObjC classes with non-POD C++ ivars are specially marked in the metadata.

// { dg-do run { target *-*-darwin* } }
// { dg-skip-if "" { *-*-* } { "-fgnu-runtime" } { "" } } 
// { dg-options "-fobjc-call-cxx-cdtors" }
// { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } }

#include "../objc-obj-c++-shared/Object1.h"
#include "../objc-obj-c++-shared/next-mapping.h"

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

  cls = objc_get_class("Foo");
#if NEXT_OBJC_USE_NEW_INTERFACE
  CHECK_IF(class_isMetaClass(cls) & CLS_HAS_CXX_STRUCTORS);
  cls = objc_getClass("Bar");
  CHECK_IF(!(class_isMetaClass(cls) & CLS_HAS_CXX_STRUCTORS));
#else
  CHECK_IF(cls->info & CLS_HAS_CXX_STRUCTORS);
  cls = objc_getClass("Bar");
  CHECK_IF(!(cls->info & CLS_HAS_CXX_STRUCTORS));
#endif
  return 0;
}
#include "../objc-obj-c++-shared/Object1-implementation.h"
