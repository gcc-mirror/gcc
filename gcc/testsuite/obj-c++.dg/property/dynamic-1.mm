/* { dg-do compile } */

#include <objc/objc.h>

@interface MyRootClass
{
  Class isa;
}
@end

@implementation MyRootClass
@end

@dynamic isa;           /* { dg-error "misplaced .@dynamic. Objective-C.. construct" } */

@interface Test : MyRootClass
{
  int v1;
  int v2;
  int v3;
  int v4;
}
@property int v1;
@property int v2;
@property int v3;
@property int v4;
@end

@implementation Test
@dynamic;            /* { dg-error "expected identifier" } */
@dynamic v1, ;       /* { dg-error "expected identifier" } */
@dynamic v1, v2, v3;
@dynamic v4;        
@end
