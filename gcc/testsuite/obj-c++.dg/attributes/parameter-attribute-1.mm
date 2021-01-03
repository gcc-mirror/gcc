/* Test __attribute__((unused)) for an Objective-C method parameter.  */
/* { dg-do compile } */
/* { dg-options "-Wunused-parameter" } */
/* { dg-additional-options "-Wno-objc-root-class" } */

#include <objc/objc.h>

@interface MyRootClass
{
  Class isa;
}
- (id) method1: (id) argument1;
- (id) method2: (id) __attribute__((unused)) argument1;
- (id) method3: (id) __attribute__((unused)) argument1
   andArgument: (id) argument2;
- (id) method4: (id) __attribute__((unused)) argument1
   andArgument: (id) __attribute__((unused)) argument2;
- (id) method5: (id) argument1
   andArgument: (id) __attribute__ ((unused)) argument2;
@end

@implementation MyRootClass
- (id) method1: (id) argument1  /* { dg-warning "unused parameter .argument1." } */
{
  return nil;
}
- (id) method2: (id) __attribute__((unused)) argument1
{
  return nil;
}
- (id) method3: (id) __attribute__((unused)) argument1
   andArgument: (id) argument2 /* { dg-warning "unused parameter .argument2." } */
{
  return nil;
}
- (id) method4: (id) __attribute__((unused)) argument1
   andArgument: (id) __attribute__((unused)) argument2
{
  return nil;
}
- (id) method5: (id) argument1
   andArgument: (id) __attribute__ ((unused)) argument2 /* { dg-warning "unused parameter .argument1." } */
{
  return nil;
}
@end
