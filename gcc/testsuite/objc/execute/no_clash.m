/* Contributed by Nicola Pero - Fri Mar  9 19:39:15 CET 2001 */
#import "../../objc-obj-c++-shared/Object1.h"
#import <objc/objc.h>

/* Test that using the same name for different things makes no 
   problem */

@interface TestClass : Object
{
  int test;
}
+ (int) test;
- (int) test;
@end

@implementation TestClass
+ (int) test
{
  return 1;
}
- (int) test
{
  /* 0 + 2 as `test' is implicitly initialized to zero */
  return test + 2; 
}
@end


int main (void)
{
  if ([TestClass test] != 1)
    {
      abort ();
    }
  if ([[[TestClass alloc] init] test] != 2)
    {
      abort ();
    }

  return 0;
}
#include "../../objc-obj-c++-shared/Object1-implementation.h"
