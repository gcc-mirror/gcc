/* { dg-do compile } */
#include <objc/objc.h>

@interface MyRootClass
{
  Class isa;
}
@end

@interface MySubClass
{
  volatile int a;  /* This is allowed */
  extern int b;    /* { dg-error "expected" } */
  static int c;    /* { dg-error "expected" } */
  inline int d;    /* { dg-error "expected" } */
  typedef int e;   /* { dg-error "expected" } */
  __thread int f;  /* { dg-error "expected" } */
}
@end
