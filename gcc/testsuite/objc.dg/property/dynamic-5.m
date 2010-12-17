/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

/* Test @dynamic in the real scenario where a class declares a
   @property, uses @dynamic to avoid implementing it, then subclasses
   implement it.  */

#include <objc/objc.h>
#include <objc/runtime.h>
#include <stdlib.h>

@interface MyRootClass
{
  Class isa;
}
@property int a;
+ (id) initialize;
+ (id) alloc;
- (id) init;
@end

@implementation MyRootClass
+ (id) initialize { return self; }
+ (id) alloc { return class_createInstance (self, 0); }
- (id) init { return self; }
@dynamic a;
@end

@interface Test : MyRootClass
{
  int v1;
}
@end

@implementation Test
@synthesize a = v1;
@end

int main (void)
{
  /* Note how 'object' is declared to be of class 'MyRootClass', but
     actually is of the subclass which implements the property for
     real.  */
  MyRootClass *object = [[Test alloc] init];

  object.a = 40;

  if (object.a != 40)
    abort ();

  return 0;
}
