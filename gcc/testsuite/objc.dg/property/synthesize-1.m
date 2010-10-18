/* { dg-do compile } */

#include <objc/objc.h>

@interface MyRootClass
{
  Class isa;
}
@end

@implementation MyRootClass
@end

@synthesize isa;                    /* { dg-error ".@synthesize. not in @implementation context" } */

@interface Test : MyRootClass
{
  int v1;
  int v2;
  int v3;
  int v4;
}
@end

@implementation Test
@synthesize;                        /* { dg-error "expected identifier" } */
@synthesize v1, ;                   /* { dg-error "expected identifier" } */
@synthesize v1, v2 = ;              /* { dg-error "expected identifier" } */
@synthesize v1, v2=v2, v3 = v3,v4;  /* { dg-error ".@synthesize. is not supported in this version of the compiler" } */
@synthesize v4;                     /* { dg-error ".@synthesize. is not supported in this version of the compiler" } */
@end
