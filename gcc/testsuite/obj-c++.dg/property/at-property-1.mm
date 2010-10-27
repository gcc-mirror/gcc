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
/* FIXME - there is a problem with the testuite in running the following test.  The compiler
   generates the messages, but the testsuite still complains.  */
/*@property ( int i;*/          /* dg-error "unknown property attribute" */
                                /* dg-error "expected ... "       "" { target *-*-* } 18 */
                                /* dg-error "expected identfier " "" { target *-*-* } 18 */
@end
