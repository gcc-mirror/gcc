/* { dg-do compile } */

#include <objc/objc.h>

@interface MyRootClass
{
  Class isa;
}
@property;                      /* { dg-error "expected identifier" } */
@property int;                  /* { dg-error "expected identifier" } */
@property int a;
@property int b, c;
@property () int d;             /* { dg-error "expected identifier" } */
@property (readonly) int e;
@property (readonly,) int f;    /* { dg-error "expected identifier" } */
@property (xxx) int g;          /* { dg-error "unknown property attribute" } */
@property (readonly,xxx) int h; /* { dg-error "unknown property attribute" } */
@property ( int i;              /* { dg-error "expected identifier" } */
                                /* { dg-error "expected ... "       "" { target *-*-* } .-1 } */
@end
