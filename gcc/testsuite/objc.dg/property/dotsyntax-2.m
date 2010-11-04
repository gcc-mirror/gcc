/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

/* Test the 'dot syntax' without a declarated property.  This tests the case where
   only the setter (or only the getter) exists.  */

#include <stdlib.h>
#include <objc/objc.h>
#include <objc/runtime.h>

@interface MyRootClass
{
  Class isa;
  int a;
  id b;
}
+ (id) initialize;
+ (id) alloc;
- (id) init;
- (int) a;
- (void) setCount: (int)value;
- (id) b;
- (void) setNext: (id)value;
@end

@implementation MyRootClass
+ (id) initialize { return self; }
+ (id) alloc { return class_createInstance (self, 0); }
- (id) init { return self; }
- (int) a
{
  return a;
}
- (void) setCount: (int)value
{
  a = value;
}
- (id) b
{
  return b;
}
- (void) setNext: (id)value
{
  b = value;
}
@end

int main (void)
{
  MyRootClass *object = [[MyRootClass alloc] init];

  /* This should work because -setCount: exists (even if -count does
     not).  */
  object.count = 40;

  /* This should work because -a exists (even if -setA: does not).  */
  if (object.a != 40)
    abort ();

  /* This should work because -setNext: exists (even if -next does
     not).  */
  object.next = object;

  /* This should work because -b exists (even if -setB: does not).  */
  if (object.b != object)
    abort ();

  return 0;
}


