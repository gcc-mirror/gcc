/* { dg-do compile } */

#include <objc/objc.h>

@interface MyRootClass
{
  Class isa;
}
@property volatile int a;  /* This is allowed */
@property extern int b;    /* { dg-error "expected" } */
@property static int c;    /* { dg-error "expected" } */
@property inline int d;    /* { dg-error "expected" } */
@property typedef int e;   /* { dg-error "expected" } */
@property __thread int f;  /* { dg-error "expected" } */
@end
