/* Contributed by Nicola Pero - Fri Mar  9 19:39:15 CET 2001 */
#include <objc/objc.h>
#include <objc/objc-api.h>

/* Test the hidden argument _cmd to method calls */

@interface TestClass 
{
  Class isa;
}
+ (const char*) method;
@end

@implementation TestClass
+ (const char*) method;
{
  return sel_get_name (_cmd);
}
@end


int main (void)
{
  if (strcmp ([TestClass method], "method"))
    {
      abort ();
    }

  return 0;
}
