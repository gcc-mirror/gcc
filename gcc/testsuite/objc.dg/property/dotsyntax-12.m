/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

/* Test looking up a setter or getter which are in a protocol attached
   to a category of a superclass.  */

#include <stdlib.h>
#include <objc/objc.h>
#include <objc/runtime.h>

static int c;

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

@protocol count
- (int) count;
- (void) setCount: (int)count;
@end

@protocol classCount
+ (int) classCount;
+ (void) setClassCount: (int)count;
@end

@interface MyRootClass (Category) <count, classCount>
@end

@implementation MyRootClass (Category)
- (int) count
{
  return a;
}
- (void) setCount: (int)count
{
  a = count;
}
+ (int) classCount
{
  return c;
}
+ (void) setClassCount: (int)count
{
  c = count;
}
@end

@interface MySubClass : MyRootClass
+ (int) testMe;
- (int) testMe;
@end

@implementation MySubClass
- (int) testMe
{
  self.count = 400;
  if (self.count != 400)
    abort ();             

  return self.count;
}
+ (int) testMe
{
  self.classCount = 4000;
  if (self.classCount != 4000)
    abort ();

  return self.classCount;
}
@end

int main (void)
{
  MySubClass *object = [[MySubClass alloc] init];

  object.count = 44;
  if (object.count != 44)
    abort ();

  MySubClass.classCount = 40;
  if (MySubClass.classCount != 40)
    abort ();

  if ([object testMe] != 400)
    abort ();

  if ([MySubClass testMe] != 4000)
    abort ();

  return 0;
}
