/* Test __attribute__ ((unavailable)) */
/* { dg-do compile } */
/* { dg-options "" } */

#include <objc/objc.h>

@interface MyClass
{
  Class isa;
}
+ (int) unavailableClassMethod: (id)firstObject, ...    __attribute__((sentinel)) __attribute__((unavailable));
- (int) unavailableInstanceMethod: (id)firstobject, ... __attribute__((sentinel)) __attribute__((unavailable));
@end

/* Test that unavailability errors are produced even if the method is
   also marked with another attribute too (this is to test the
   processing of multiple attributes).  */
void foo (void)
{
  MyClass *object = nil;

  [MyClass unavailableClassMethod: object, nil];           /* { dg-error "is unavailable" } */
  [object unavailableInstanceMethod: object, nil];         /* { dg-error "is unavailable" } */
}
