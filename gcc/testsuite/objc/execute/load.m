/* Contributed by Nicola Pero - Wed Mar  7 17:55:04 CET 2001 */
#include <objc/objc.h>

/* Test that +load is automatically called before main is run */

static int static_variable = 0;

@interface TestClass
{
  Class isa;
}
+ (void) load;
@end

@implementation TestClass
+ (void) load
{
  static_variable = 1;
}
@end

int main (void)
{
  if (static_variable != 1)
    {
      abort ();
    }

  return 0;
}
