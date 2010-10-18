/* { dg-do compile } */

#include <objc/objc.h>

@interface MyRootClass
{
  Class isa;
}
@end

@implementation MyRootClass
@end

@dynamic isa;           /* { dg-error ".@dynamic. not in @implementation context" } */

@interface Test : MyRootClass
{
  int v1;
  int v2;
  int v3;
  int v4;
}
@end

@implementation Test
@dynamic;            /* { dg-error "expected identifier" } */
@dynamic v1, ;       /* { dg-error "expected identifier" } */
@dynamic v1, v2, v3; /* { dg-error ".@dynamic. is not supported in this version of the compiler" } */
@dynamic v4;         /* { dg-error ".@dynamic. is not supported in this version of the compiler" } */
@end
