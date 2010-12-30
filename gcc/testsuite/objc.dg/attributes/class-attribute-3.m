/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, December 2010.  */
/* { dg-do compile } */

/* Test that you get a warning when an unknown class attribute is ignored.  */

#include <objc/objc.h>

__attribute__ ((unknown_attribute))
@interface MyClass
{ /* { dg-warning "ignored" } */
  Class isa;
}
+ (id) new;
@end
