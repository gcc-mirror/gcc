/* { dg-do compile } */
// { dg-additional-options "-Wno-objc-root-class" }
#include <objc/objc.h>

@interface MyRootClass
{
  Class isa;
}
@end

@interface MySubClass
{
  volatile int a;  /* This is allowed */
  extern int b;    /* { dg-error "invalid type" } */
  static int c;    /* { dg-error "invalid type" } */
  inline int d;    /* { dg-error "declared as an .inline." } */
  typedef int e;   /* { dg-error "invalid type" } */
  __thread int f;  /* { dg-error "invalid type" } */
}
@end
