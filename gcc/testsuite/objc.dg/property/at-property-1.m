/* { dg-do compile } */

#include <objc/objc.h>

@interface MyRootClass
{
  Class isa;
}
@property;                      /* { dg-error "expected" } */
@property int;                  /* { dg-error "expected identifier" } */
                                /* { dg-warning "declaration does not declare anything" "" { target *-*-* } 10 } */
@property int a;
@property int b, c;
@property () int d;             /* { dg-error "expected identifier" } */
@property (readonly) int e;
@property (readonly,) int f;    /* { dg-error "expected identifier" } */
@property (xxx) int g;          /* { dg-error "unknown property attribute" } */
@property (readonly,xxx) int h; /* { dg-error "unknown property attribute" } */
@property ( int i;              /* { dg-error "unknown property attribute" } */
/* Because the last syntax error opens a '(' and never closes it, we get to the end of input.  */
@end                            /* { dg-error "expected ..end. at end of input" } */
