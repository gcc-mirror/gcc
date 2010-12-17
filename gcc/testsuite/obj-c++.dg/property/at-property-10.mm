/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, October 2010.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

/* Test the property syntax in a number of expressions.  */

#include <stdlib.h>
#include <objc/objc.h>
#include <objc/runtime.h>

@interface MyRootClass
{
  Class isa;
  int a;
}
@property (nonatomic) int a;
+ (id) initialize;
+ (id) alloc;
- (id) init;
@end

@implementation MyRootClass
+ (id) initialize { return self; }
+ (id) alloc { return class_createInstance (self, 0); }
- (id) init { return self; }
@synthesize a;
@end

int
test (int g)
{
  return g;
}

int main (void)
{
  MyRootClass *object = [[MyRootClass alloc] init];
  MyRootClass *object2 = [[MyRootClass alloc] init];

  object.a = 14;
  object.a = object.a + object.a;

  if (object.a != 28)
    abort ();

  object.a = 99;
  object.a++;

  if (object.a != 100)
    abort ();

  object.a = 99;
  object.a *= 2;

  if (object.a != 198)
    abort ();

  {
    int f = object.a;

    if (f != 198)
      abort ();

    if (f != object.a)
      abort ();

    if (object.a != f)
      abort ();

    object.a = object.a;

    if (object.a != 198)
      abort ();
  }  

  if (test (object.a) != 198)
    abort ();

  object.a = -object.a;

  if (object.a != -198)
    abort ();

  for (object.a = 0; object.a < 99; object.a++)
    object2.a = object.a;

  if (object2.a != object.a - 1)
    abort ();

  if (object2.a != 98)
    abort ();

  if (object.a != 99)
    abort ();

  return (0);
}
