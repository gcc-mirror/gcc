// Check if ObjC classes with non-POD C++ ivars are specially marked in the metadata.

// { dg-do run { target *-*-darwin* } }
// { dg-skip-if "" { *-*-* } { "-fgnu-runtime" } { "" } }
// { dg-skip-if "Headers incompatible with 10.4 APIs" { *-*-darwin1[3-8]* } { "-fnext-runtime" } { "" } }
// { dg-additional-options "-fobjc-call-cxx-cdtors -mmacosx-version-min=10.4 -framework Foundation" }
// This test has no equivalent or meaning for m64/ABI V2
// { dg-xfail-run-if "No Test Avail" {  *-*-darwin* && lp64 } { "-fnext-runtime" } { "" } }

#include <objc/objc-runtime.h>
#include <stdlib.h>
#include "../objc-obj-c++-shared/F-NSObject.h"

//extern "C" { int printf(const char *,...); }
#define CHECK_IF(expr) if(!(expr)) abort()

#ifndef CLS_HAS_CXX_STRUCTORS
#define CLS_HAS_CXX_STRUCTORS 0x2000L
#endif

struct cxx_struct {
  int a, b;
  cxx_struct (void) { a = b = 55; }
};

@interface Foo: NSObject {
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
#ifndef __LP64__
  Class cls;

  cls = objc_getClass("Foo");
//  printf((const char *)"Foo info %lx\n",cls->info);
  CHECK_IF((cls->info & CLS_HAS_CXX_STRUCTORS) != 0);
  cls = objc_getClass("Bar");
//  printf((const char *)"Bar info %lx\n",cls->info);
  CHECK_IF((cls->info & CLS_HAS_CXX_STRUCTORS) == 0);

#else
  /* No test needed or available.  */
  abort ();
#endif
  return 0;
}
