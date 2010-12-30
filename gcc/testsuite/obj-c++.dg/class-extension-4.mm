/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, December 2010.  */
/* { dg-do compile } */

/* This test tests you can not declare a class extension after the class @implementation.  */

#include <objc/objc.h>

@interface MyObject
{
  Class isa;
}
@end

@implementation MyObject
@end

@interface MyObject () /* { dg-error "class extension for class .MyObject. declared after its ..implementation." } */
- (void) test;
@end
