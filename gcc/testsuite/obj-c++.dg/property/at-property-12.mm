/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, October 2010.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

/* Test atomic, assign synthesized methods.  */

#include <stdlib.h>
#include <objc/objc.h>
#include <objc/runtime.h>

@interface MyRootClass
{
  Class isa;
  int a;
  id b;
}
@property int a;
@property (assign) id b;
+ (id) initialize;
+ (id) alloc;
- (id) init;
@end

@implementation MyRootClass
+ (id) initialize { return self; }
+ (id) alloc { return class_createInstance (self, 0); }
- (id) init { return self; }
@synthesize a;
@synthesize b;
@end

int main (void)
{
  MyRootClass *object = [[MyRootClass alloc] init];

  object.a = 40;
  if (object.a != 40)
    abort ();

  object.b = object;
  if (object.b != object)
    abort ();

  return (0);
}


