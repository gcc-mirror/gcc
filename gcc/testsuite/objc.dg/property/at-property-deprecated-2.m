/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, October 2010.  */
/* { dg-do compile } */

/* Test that deprecation warnings are produced when a setter/getter of
   a @property is used directly.  */

#include <objc/objc.h>

@interface MyClass
{
  Class isa;
  int variable;
} 
@property (assign, nonatomic) int property __attribute__ ((deprecated));
@end

void foo (void)
{
  MyClass *object = nil;

  if ([object property] > 0)  /* { dg-warning "is deprecated" } */
    {
      [object setProperty: 43]; /* { dg-warning "is deprecated" } */ 
    }
}
