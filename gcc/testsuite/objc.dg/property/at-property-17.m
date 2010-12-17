/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do compile } */

#include <objc/objc.h>

/* Test that if you have a property declared in a class, with
   getters/setters in the superclass, there are no warnings.  */

@interface MyRootClass
{
  Class isa;
  int myCount;
  int myCount2;
  int myCount3;
}
- (int)count;
- (void)setCount: (int)number;
- (int)count2;
- (void)setCount2: (int)number;
- (int)count3;
@end

@implementation MyRootClass
- (int) count
{
  return myCount;
}
- (void) setCount: (int)number
{
  myCount = number;
}
- (int) count2
{
  return myCount2;
}
- (void) setCount2: (int)number
{
  myCount2 = number;
}
- (int) count3
{
  return myCount3;
}
@end



/* Try with a subclass.  */
@interface MyClass : MyRootClass
@property int count;
@end

@implementation MyClass
@end /* No warnings.  */



/* Try with a category.  */
@interface MyRootClass (count)
@property int count;
@end

@implementation MyRootClass (count)
@end /* No warnings.  */



/* Try with a category of a subclass.  */
@interface MyClass2 : MyClass
@end

@implementation MyClass2
@end

@interface MyClass2 (count2)
@property int count2;
@end

@implementation MyClass2 (count2)
@end /* No warnings.  */



/* Now, try with a category of a subclass, but with a missing setter,
   which should generate a warning.  */
@interface MyClass3 : MyClass
@end

@implementation MyClass3
@end

@interface MyClass3 (count3)
@property int count3;
@end

@implementation MyClass3 (count3)
@end /* { dg-warning "incomplete implementation" } */
/* { dg-warning "method definition for .-setCount3:. not found" "" { target *-*-* } 97 } */
