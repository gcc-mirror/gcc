/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do compile } */
// { dg-additional-options "-Wno-objc-root-class" }

#include <objc/objc.h>

/* Test that @optional for @protocol works.  */

@protocol MyProtocol
+ (int)classMethod;
- (int)method;
@optional
+ (int)optionalClassMethod;
- (int)optionalMethod;
@end

@interface MyRootClass <MyProtocol>
@end

/* The implementation implements both the @required methods, but none
   of the @optional ones.  There should be no warnings as the
   @optional methods are optional. ;-)  */
@implementation MyRootClass
+ (int)classMethod
{
  return 20;
}
- (int)method
{
  return 11;
}
@end

int function (id <MyProtocol> object1,
	      MyRootClass *object2)
{
  /* Test that there are no warnings if you try to use an @optional
     method with an object of the class.  */
  int i = 0;

  i += [object1 method];
  i += [object2 method];
  i += [MyRootClass classMethod];
  i += [object1 optionalMethod];
  i += [object2 optionalMethod];
  i += [MyRootClass optionalClassMethod];

  return i;
}
