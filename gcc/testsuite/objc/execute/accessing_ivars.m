/* Contributed by Nicola Pero - Thu Mar  8 16:27:46 CET 2001 */
#include <stdlib.h>
#include "../../objc-obj-c++-shared/TestsuiteObject.m"

/* Test that by using -> we can access ivars of other objects of the same 
   class */

@interface TestClass : TestsuiteObject
{
  int value;
}
- (int) value;
- (void) setValue: (int)number;
- (void) takeValueFrom: (TestClass *)object;
@end

@implementation TestClass : TestsuiteObject
{
  int value;
}
- (int) value
{ 
  return value;
}
- (void) setValue: (int)number
{
  value = number;
}
- (void) takeValueFrom: (TestClass *)object
{
  value = object->value;
}
@end

int main (void)
{
  TestClass *a;
  TestClass *b;

  a = [TestClass new];
  [a setValue: 10];
  
  b = [TestClass new];
  [b setValue: -10];

  [b takeValueFrom: a];

  if ([b value] != [a value])
    {
      abort ();
    }

  return 0;
}

