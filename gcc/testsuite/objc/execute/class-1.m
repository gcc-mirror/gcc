/* Contributed by Nicola Pero - Tue Mar  6 23:05:53 CET 2001 */
#include <objc/objc.h>
#include <objc/objc-api.h>

/* Tests creating a RootClass */

@interface RootClass
{
  Class isa;
}
@end

@implementation RootClass
@end

#include "class-tests-1.h"

int main (void)
{
  test_class_with_superclass ("RootClass", "");

  return 0;
}
