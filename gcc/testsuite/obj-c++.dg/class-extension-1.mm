/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, December 2010.  */
/* { dg-do compile } */

/* This test tests the basic of class extensions.  */

#include <objc/objc.h>

@interface MyObject
{
  Class isa;
}
- (int) test;
@end

@interface MyObject ()
- (int) test2;
- (int) test3;
@end

@implementation MyObject
- (int) test
{
  return 20;
}
- (int) test2
{
  return 20;
}
@end /* { dg-warning "incomplete implementation of class .MyObject." } */
     /* { dg-warning "method definition for .-test3. not found" "" { target *-*-* } 29 } */
