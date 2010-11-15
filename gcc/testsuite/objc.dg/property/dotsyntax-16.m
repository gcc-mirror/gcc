/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

/* Test dot-syntax with pre/post increment and decrement.  */

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

int main (void)
{
  MyRootClass *object = [[MyRootClass alloc] init];

  object.count = 10;
  if (object.count != 10)
    abort ();

  /* First, test that they increment/decrement as expected.  */
  object.count++;
  if (object.count != 11)
    abort ();

  ++object.count;
  if (object.count != 12)
    abort ();

  object.count--;
  if (object.count != 11)
    abort ();

  --object.count;
  if (object.count != 10)
    abort ();

  /* Now, test that they are pre/post increment/decrement, as
     expected.  */
  if (object.count++ != 10)
    abort ();

  if (object.count != 11)
    abort ();

  if (++object.count != 12)
    abort ();

  if (object.count != 12)
    abort ();

  if (object.count-- != 12)
    abort ();

  if (object.count != 11)
    abort ();

  if (--object.count != 10)
    abort ();

  if (object.count != 10)
    abort ();

  return 0;
}


