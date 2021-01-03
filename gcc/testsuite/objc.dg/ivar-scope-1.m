/* Test instance variable scope.  */
/* Author: Dimitris Papavasiliou <dpapavas@gmail.com>.  */
/* { dg-do compile } */
#include <objc/objc.h>

#if defined(__has_attribute) && __has_attribute(objc_root_class)
__attribute__((objc_root_class))
#endif
@interface MyClass
{
  int someivar;
}
- (void) test;
@end

@implementation MyClass
- (void) test
{
  int a;

  /* Make sure instance variables do have local scope when
     -fno-local-ivar isn't specified. */
  
  a = self->someivar;  /* No warning or error. */
  a = someivar;        /* No error. */
}
@end
