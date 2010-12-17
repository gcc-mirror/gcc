/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

/* Test dot-syntax with tricky assignments.  */

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
- (int) somethingToExecuteOnlyOnce;
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
- (int) somethingToExecuteOnlyOnce
{
  a++;
  return 10;
}
@end

int main (void)
{
  MyRootClass *object1 = [[MyRootClass alloc] init];
  MyRootClass *object2 = [[MyRootClass alloc] init];
  MyRootClass *object3 = [[MyRootClass alloc] init];
  int i;

  object1.count = 10;
  if (object1.count != 10)
    abort ();

  object2.count = 10;
  if (object2.count != 10)
    abort ();

  /* Test multiple assignments to a constant.  */
  object1.count = object2.count = 20;

  if (object1.count != 20 || object2.count != 20)
    abort ();

  i = object1.count = 30;

  if (i != 30 || object1.count != 30)
    abort ();

  i = object2.count = 30;

  if (i != 30 || object2.count != 30)
    abort ();

  /* Test a simple assignment to something with a side-effect; the
     'rhs' should be evaluated only once.  */
  object1.count = ([object2 somethingToExecuteOnlyOnce] > 0 ? 30 : 45);

  if (object1.count != 30 || object2.count != 31)
    abort ();

  /* Test multiple assignments with side effects.  */
  object3.count = object1.count = ([object2 somethingToExecuteOnlyOnce] > 0 ? 30 : 45);

  if (object1.count != 30 || object2.count != 32 || object3.count != 30)
    abort ();

  return 0;
}


