/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, February 2011.  */
/* { dg-do compile } */

/* Test that the 'objc_exception' attribute is accepted for
   @interfaces, but not for anything else.  */

#include <objc/objc.h>

/* Fine.  */
__attribute__ ((objc_exception))
@interface MyClass
{
  Class isa;
}
@end

/* Fine.  */
__attribute__ ((__objc_exception__))
@interface MyClass2
{
  Class isa;
}
@end

__attribute__ ((objc_exception))
@protocol MyProtocol; /* { dg-warning "ignored" } */

__attribute__ ((objc_exception))
int myVariable; /* { dg-warning "ignored" } */

__attribute__ ((objc_exception))
int myFunction (int argument); /* { dg-warning "ignored" } */
