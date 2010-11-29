/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

/* Test dot-syntax with super in a category.  */

#include <stdlib.h>
#include <objc/objc.h>
#include <objc/runtime.h>

@interface MyRootClass
{
  Class isa;
  int a;
}
+ (id) initialize;
+ (id) alloc;
- (id) init;
- (int) count;
- (void) setCount: (int)count;
@end

@implementation MyRootClass
+ (id) initialize { return self; }
+ (id) alloc { return class_createInstance (self, 0); }
- (id) init { return self; }
- (int) count
{
  return a;
}
- (void) setCount: (int)count
{
  a = count;
}
@end

/* First, test 'super' in the main implementation of a subclass.  */
@interface MySubClass : MyRootClass
- (int) superCount;
- (void) setSuperCount: (int)count;
@end

@implementation MySubClass
- (int) superCount
{
  return super.count;
}
- (void) setSuperCount: (int)count
{
  super.count = count;
}
@end

/* Now, test 'super' in a category of a subclass.  */
@interface MySubClass (Category)
- (int) superCount2;
- (void) setSuperCount2: (int)count;
- (int) test: (int)x;
@end

@implementation MySubClass (Category)
- (int) superCount2
{
  return super.count;
}
- (void) setSuperCount2: (int)count
{
  super.count = count;
}
- (int) test: (int)x
{
  /* For positive x, the following will leave super.count
     unchanged.  */
  super.count++;
  --super.count;

  super.count = (x < 0 ? x : super.count);
  
  if ((x = super.count))
    super.count += 1;

  if ((x = super.count))
    super.count -= 1;

  /* Finally, also put a bit of self.count in the mix.  */
  self.count++;
  super.count--;

  return super.count;
}
@end

int main (void)
{
  MySubClass *object = [[MySubClass alloc] init];

  object.count = 10;
  if (object.count != 10)
    abort ();

  object.superCount = 11;
  if (object.superCount != 11)
    abort ();

  object.superCount2 = 12;
  if (object.superCount2 != 12)
    abort ();

  if ([object test: 45] != 12)
    abort ();

  return 0;
}
