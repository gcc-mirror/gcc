/* Test __attribute__ ((unavailable)) */
/* { dg-do compile } */
/* { dg-options "" } */

#include <objc/objc.h>

/* Test that __attribute__ ((__unavailable__)) works as well as __attribute__ ((unavailable)).  */
@interface MyClass
{
  Class isa;
}
+ (int) unavailableClassMethod: (id)firstObject, ...    __attribute__((__unavailable__));
- (int) unavailableInstanceMethod: (id)firstobject, ... __attribute__((__unavailable__));
@end

void foo (void)
{
  MyClass *object = nil;

  [MyClass unavailableClassMethod: object, nil];           /* { dg-error "is unavailable" } */
  [object unavailableInstanceMethod: object, nil];         /* { dg-error "is unavailable" } */
}
