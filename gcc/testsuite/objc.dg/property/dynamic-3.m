/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do compile } */

#include <objc/objc.h>

@interface MyRootClass
{
  Class isa;
}
@end

@implementation MyRootClass
@end

/* Test @property/@dynamic in a category.  First, a case where
   @dynamic should turn off all warnings.  */

@interface MyRootClass (Category)
@property int a;
- (int) test;
@end
@implementation MyRootClass (Category)
@dynamic a;
- (int) test
{
  return self.a; /* This should compile into [self a] with no warnings.  */
}
@end



/* Test @property/@dynamic in a category.  Second, a case with a
   missing setter and no @dynamic.  A warning should be generated.  */

@interface MyRootClass (Category2)
@property int b;
- (int) test;
@end
@implementation MyRootClass (Category2)
- (int) b
{
  return 0;
}
- (int) test
{
  return self.b;
}
@end /* { dg-warning "incomplete implementation" } */
/* { dg-warning "method definition for .-setB:. not found" "" { target *-*-* } 48 } */
