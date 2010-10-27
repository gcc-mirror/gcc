/* { dg-do compile } */

#include <objc/objc.h>

@interface MyRootClass
{
  Class isa;
}
@property volatile int a;  /* This is allowed */
@property extern int b;    /* { dg-error "invalid type" } */
@property static int c;    /* { dg-error "invalid type" } */
@property inline int d;    /* { dg-error "declared as an .inline." } */
@property typedef int e;   /* { dg-error "invalid type" } */
@property __thread int f;  /* { dg-error "invalid type" } */
@end
