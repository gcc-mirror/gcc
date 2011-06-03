/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, May 2011.  */
/* { dg-do compile } */
/* { dg-options "-pedantic" } */

#include <objc/objc.h>

@interface MyClass
{
  ;       /* { dg-warning "extra semicolon" } */
  int a;  
  ;       /* { dg-warning "extra semicolon" } */
  int b;
  ;       /* { dg-warning "extra semicolon" } */
}
@end
