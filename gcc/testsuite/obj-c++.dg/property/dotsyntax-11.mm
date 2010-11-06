/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do compile } */

/* Test the error reporting for the dot-syntax in the scenario where
   we have a setter, but not a getter, yet a getter is requested.  */

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
- (void) setCount: (int)count;
+ (void) setClassCount: (int)count;
@end

@implementation MyRootClass
+ (id) initialize { return self; }
+ (id) alloc { return class_createInstance (self, 0); }
- (id) init { return self; }
- (void) setCount: (int)count
{
  a = count;
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
  super.count = 400;
  if (super.count != 400) /* { dg-error "no .count. getter found" } */
    abort ();             

  return super.count;     /* { dg-error "no .count. getter found" } */
}
+ (int) testMe
{
  super.classCount = 4000;
  if (super.classCount != 4000) /* { dg-error "no .classCount. getter found" } */
    abort ();

  return super.classCount;      /* { dg-error "no .classCount. getter found" } */
}
@end
