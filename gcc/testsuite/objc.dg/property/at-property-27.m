/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, December 2010.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

/* Test overriding a readonly @property with a readwrite one in a class extension.  */

#include <stdlib.h>
#include <objc/objc.h>
#include <objc/runtime.h>

@interface MyRootClass
{
  Class isa;
}
+ (id) initialize;
+ (id) alloc;
- (id) init;
@end

@implementation MyRootClass
+ (id) initialize { return self; }
+ (id) alloc { return class_createInstance (self, 0); }
- (id) init { return self; }
@end

@protocol count2
/* Use a different getters/setters, so that the only way to compile
   object.countX is to find the actual @property.  */
@property (readonly, getter=number2) int count2;
@end

@interface MySubClass : MyRootClass
{
  int count1;
  int count2;
}
@property (readonly, getter=number1) int count1;
@end

@interface MySubClass ()
@property (readwrite, getter=number1, setter=setNumber1:) int count1;
@end

@interface MySubClass ()  <count2>
@property (readwrite, getter=number2, setter=setNumber2:) int count2;
@end

@implementation MySubClass
@synthesize count1;
@synthesize count2;
@end

int main (void)
{
  MySubClass *object = [[MySubClass alloc] init];

  object.count1 = 20;
  if (object.count1 != 20)
    abort ();

  object.count2 = 11;
  if (object.count2 != 11)
    abort ();

  return 0;
}
