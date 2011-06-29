/* Contributed by Nicola Pero - Tue Mar  6 23:05:53 CET 2001 */

#include <objc/objc.h>

/* Tests creating a root class and a minimal subclass tree */

@interface RootClass
{
  Class isa;
}
@end

@implementation RootClass
@end

@interface SubClassA : RootClass
@end

@implementation SubClassA
@end

@interface SubClassB : RootClass
@end

@implementation SubClassB
@end

@interface SubSubClass : SubClassA
@end

@implementation SubSubClass
@end

#include "class-tests-1.h"

int main (void)
{
  test_class_with_superclass ("SubClassA", "RootClass");
  test_class_with_superclass ("SubClassB", "RootClass");
  test_class_with_superclass ("SubSubClass", "SubClassA");

  return 0;
}
