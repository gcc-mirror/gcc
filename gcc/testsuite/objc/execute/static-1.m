/* Contributed by Nicola Pero - Fri Mar  9 19:39:15 CET 2001 */
#include <objc/objc.h>

/* Test defining a static variable *inside* a class implementation */

@interface Test
{
  Class isa;
}
+ (int) test;
@end

@implementation	Test

static int test = 1;

+ (int) test
{
  return test;
}

#ifdef __NEXT_RUNTIME__                                   
+ initialize { return self; }
#endif
@end

int main (void)
{
  if ([Test test] != 1)
    {
      abort ();
    }

  return 0;
}


