/* Contributed by Nicola Pero - Wed Jun  6 14:34:23 CEST 2001 */
#include <objc/objc.h>

/* Test that +load is automatically called before main is run;
   on two different classes. */

static int static_variable1 = 0;
static int static_variable2 = 0;

@interface TestClass1
{
  Class isa;
}
+ (void) load;
@end

@implementation TestClass1
+ (void) load
{
  static_variable1 = 1;
}
@end

@interface TestClass2
{
  Class isa;
}
+ (void) load;
@end

@implementation TestClass2
+ (void) load
{
  static_variable2 = 1;
}
@end

int main (void)
{
  if (static_variable1 != 1  ||  static_variable2 != 1)
    {
      abort ();
    }

  return 0;
}
