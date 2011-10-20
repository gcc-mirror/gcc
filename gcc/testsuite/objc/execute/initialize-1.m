/* Contributed by Nicola Pero - Sat  8 Oct 2011 16:47:48 BST */
#include <objc/objc.h>

/* Test that if a class has no +initialize method, the superclass
   implementation is called.  */

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
  class_variable++;
}
+ (int) classVariable
{
  return class_variable;
}
@end

@interface TestSubClass : TestClass
@end

@implementation TestSubClass
@end

int main (void)
{
  if ([TestClass classVariable] != 1)
    {
      abort ();
    }

  if ([TestSubClass classVariable] != 2)
    {
      abort ();
    }

  return 0;
}
