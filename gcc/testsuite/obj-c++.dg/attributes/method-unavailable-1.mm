/* Test __attribute__ ((unavailable)) */
/* { dg-do compile } */
/* { dg-options "" } */

#include <objc/objc.h>

@interface MyClass
{
  Class isa;
}
+ (int) method;
- (int) method;
+ (int) unavailableClassMethod __attribute__((unavailable));
- (int) unavailableInstanceMethod __attribute__((unavailable));
@end

/* Test that deprecation warnings are produced, but not if the
   receiver is of type 'id'.  */
void foo (void)
{
  Class c;
  id object;
  MyClass *another_object;

  [c method];
  [object method];
  [c unavailableClassMethod];
  [object unavailableInstanceMethod];

  [object method];
  [another_object method];
  [MyClass unavailableClassMethod];           /* { dg-error "is unavailable" } */
  [another_object unavailableInstanceMethod]; /* { dg-error "is unavailable" } */
}
