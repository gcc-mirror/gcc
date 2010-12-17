/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

/* Test @optional @properties.  */

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

/* Use a different getters/setters, so that the only way to compile
   object.countX is to find the actual @property.  */
@protocol count
@required
/* @required + @synthesize.  */
@property (getter=number1, setter=setNumber1:) int count1;
/* @required + manual setters/getters.  */
@property (getter=number2, setter=setNumber2:) int count2;

@optional
/* @optional + @synthesize.  */
@property (getter=number3, setter=setNumber3:) int count3;
/* @optional + manual setters/getters.  */
@property (getter=number4, setter=setNumber4:) int count4;

@optional
/* @optional + readonly, with a setter added in the class itself.  */
@property (readonly, getter=number5) int count5;
@end

@interface MySubClass : MyRootClass <count>
{
  int count1;
  int count2;
  int count3;
  int count4;
  int count5;
}
- (void) setCount5: (int)value;
@end

@implementation MySubClass
@synthesize count1;
- (int) number2
{
  return count2;
}
- (void) setNumber2: (int)value
{
  count2 = value;
}
@synthesize count3;
- (int) number4
{
  return count4;
}
- (void) setNumber4: (int)value
{
  count4 = value;
}
- (int) number5
{
  return count5;
}
- (void) setCount5: (int)value
{
  count5 = value;
}
@end

int main (void)
{
  MySubClass *object = [[MySubClass alloc] init];

  /* First, test that @required and @optional properties work as
     expected if implemented either via @synthesize or manually.  */
  object.count1 = 44;
  if (object.count1 != 44)
    abort ();

  object.count2 = 88;
  if (object.count2 != 88)
    abort ();

  object.count3 = 77;
  if (object.count3 != 77)
    abort ();

  object.count4 = 11;
  if (object.count4 != 11)
    abort ();

  /* Now, test the complication: @optional @property which is
     readonly, but which has a setter manually implemented.
     Apparently it is possible to use the dotsyntax and the @optional
     @property getter is used when reading, while the manual setter is
     used when writing.  */
  object.count5 = 99;
  if (object.count5 != 99)
    abort ();

  return 0;
}
