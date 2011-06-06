/* Contributed by Nicola Pero - Tue Mar  6 23:05:53 CET 2001 */

#include <objc/objc.h>

/* Tests creating a root class and a subclass */

@interface RootClass
{
  Class isa;
}
@end

@implementation RootClass
@end

@interface SubClass : RootClass
@end

@implementation SubClass
@end

#include "class-tests-1.h"

int main (void)
{
  test_class_with_superclass ("SubClass", "RootClass");

  return 0;
}
