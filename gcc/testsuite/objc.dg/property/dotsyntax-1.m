/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

/* Test the 'dot syntax' without a declarated property.  */

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
- (int) count;
- (void) setCount: (int)value;
- (id) next;
- (void) setNext: (id)value;
@end

@implementation MyRootClass
+ (id) initialize { return self; }
+ (id) alloc { return class_createInstance (self, 0); }
- (id) init { return self; }
- (int) count
{
  return a;
}
- (void) setCount: (int)value
{
  a = value;
}
- (id) next
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

  object.count = 40;
  if (object.count != 40)
    abort ();

  object.next = object;
  if (object.next != object)
    abort ();

  return 0;
}


