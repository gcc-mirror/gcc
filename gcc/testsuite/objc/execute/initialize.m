/* Contributed by Nicola Pero - Wed Mar  7 17:55:04 CET 2001 */
#include <objc/objc.h>

/* Test that +initialize is automatically called before the class is
   accessed */

static int class_variable = 0;

@interface TestClass
{
  Class isa;
}
+ (void) initialize;
+ (int) classVariable;
@end

@implementation TestClass
+ (void) initialize
{
  class_variable = 1;
}
+ (int) classVariable
{
  return class_variable;
}
@end

int main (void)
{
  if ([TestClass classVariable] != 1)
    {
      abort ();
    }

  return 0;
}
