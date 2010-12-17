/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

/* Test the 'dot syntax' with self, both in instance and class methods.  */

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
- (int) count;
- (void) setCount: (int)count;
+ (int) classCount;
+ (void) setClassCount: (int)count;
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
+ (int) classCount
{
  return c;
}
+ (void) setClassCount: (int)count
{
  c = count;
}
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
  MyRootClass *object = [[MyRootClass alloc] init];

  if ([object testMe] != 400)
    abort ();

  if ([MyRootClass testMe] != 4000)
    abort ();

  return 0;
}


