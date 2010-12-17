/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, October 2010.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

/* Test looking up a @property in a protocol of a category of a superclass.  */

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
@end

@implementation MyRootClass
+ (id) initialize { return self; }
+ (id) alloc { return class_createInstance (self, 0); }
- (id) init { return self; }
@end

/* Use a different getter/setter, so that the only way to compile
   object.count is to find the actual @property.  */
@protocol count
@property (getter=number, setter=setNumber:) int count;
@end

@interface MySubClass : MyRootClass
- (int) testMe;
@end

@interface MySubClass (Category) <count>
@end

@implementation MySubClass (Category)
- (int) number
{
  return a;
}
- (void) setNumber: (int)count
{
  a = count;
}
@end

@implementation MySubClass
- (int) testMe
{
  self.count = 400;
  if (self.count != 400)
    abort ();             

  return self.count;
}
@end

int main (void)
{
  MySubClass *object = [[MySubClass alloc] init];

  object.count = 44;
  if (object.count != 44)
    abort ();

  if ([object testMe] != 400)
    abort ();

  return 0;
}
