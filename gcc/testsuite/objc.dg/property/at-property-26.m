/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, December 2010.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

/* Test @properties in class extensions.  */

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

@protocol count4
/* Use a different getters/setters, so that the only way to compile
   object.countX is to find the actual @property.  */
@property (getter=number4, setter=setNumber4:) int count4;
@end

@interface MySubClass : MyRootClass
{
  int count1;
  int count2;
  int count3;
  int count4;
}
@property (getter=number1, setter=setNumber1:) int count1;
@end

@interface MySubClass ()
@property (getter=number2, setter=setNumber2:) int count2;
@end

@interface MySubClass ()  <count4>
@property (getter=number3, setter=setNumber3:) int count3;
@end

@implementation MySubClass
@synthesize count1;
@synthesize count2;
- (int) number3
{
  return count3;
}
- (void) setNumber3: (int)value
{
  count3 = value;
}
@synthesize count4;
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

  object.count3 = 19;
  if (object.count3 != 19)
    abort ();

  object.count4 = 74;
  if (object.count4 != 74)
    abort ();

  return 0;
}
