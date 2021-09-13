/* Test __attribute__ ((unavailable)) */
/* { dg-do compile } */
/* { dg-options "" } */

/* Test that unavailability errors are produced when a setter/getter of
   a @property is used directly.  */

#include <objc/objc.h>

@interface MyClass
{
  Class isa;
  int variable;
}
@property (assign, nonatomic) int property __attribute__ ((unavailable));
@end

void foo (void)
{
  MyClass *object = nil;

  if ([object property] > 0)  /* { dg-error "is unavailable" } */
    {
      [object setProperty: 43]; /* { dg-error "is unavailable" } */
    }
}
