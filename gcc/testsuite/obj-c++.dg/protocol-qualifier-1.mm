/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do compile } */

/* Test that protocol qualifiers work in the same way with @class and @interface.  */

#include <objc/objc.h>

@protocol MyProtocol
- (void) method;
@end


/* This first snippet gives no warnings, which is correct as 'object'
   implements <MyProtocol> and hence responds to 'method'.  Note how
   the details of the class 'MyClass' are never used.  */
@interface MyClass
@end

void test (MyClass <MyProtocol> *object)
{
  [object method];
}


/* This second snippet should behave identically.  'object' still implements
   the same protocol and responds to 'method'.  The details of MyClass or
   MyClass2 are irrelevant.  */
@class MyClass2;

void test2 (MyClass2 <MyProtocol> *object)
{
  [object method];
}
