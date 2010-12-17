/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, October 2010.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

/* Test the property syntax with non-synthesized setter/getter
   and with a non-standard name for the setter.  */

#include <stdlib.h>
#include <objc/objc.h>
#include <objc/runtime.h>

@interface MyRootClass
{
  Class isa;
  int a;
}
@property (setter = writeA:, nonatomic) int a;
+ (id) initialize;
+ (id) alloc;
- (id) init;
@end

@implementation MyRootClass
+ (id) initialize { return self; }
+ (id) alloc { return class_createInstance (self, 0); }
- (id) init { return self; }

- (int) a
{
  return a;
}
- (void) writeA: (int)value
{
  a = value;
}
@end

int main (void)
{
  MyRootClass *object = [[MyRootClass alloc] init];

  object.a = 14;

  if (object.a != 14)
    abort ();

  object.a = 23;

  if (object.a != 23)
    abort ();

  object.a = 78;

  if (object.a != 78)
    abort ();

  return (0);
}
