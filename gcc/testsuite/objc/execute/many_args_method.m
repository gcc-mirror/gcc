/* Contributed by Nicola Pero - Fri Mar  9 19:39:15 CET 2001 */
#include <objc/objc.h>

/* Test the syntax of methods with many arguments */

@interface TestClass
{
  Class isa;
}
+ (int) sumInteger: (int)a   withInteger: (int)b;
+ (int) sum: (int)a   : (int)b;
+ (int) sumInteger: (int)a   withInteger: (int)b  withInteger: (int)c;
+ (int) sum: (int)a   : (int)b  : (int)c;
@end

@implementation TestClass
+ (int) sumInteger: (int)a  withInteger: (int)b
{
  return a + b;
}
+ (int) sum: (int)a   : (int)b
{
  return [self sumInteger: a  withInteger: b];
}
+ (int) sumInteger: (int)a   withInteger: (int)b  withInteger: (int)c
{
  return a + b + c;
}
+ (int) sum: (int)a   : (int)b  : (int)c
{
  return [self sumInteger: a  withInteger: b  withInteger: c];
}
@end


int main (void)
{
  if ([TestClass sumInteger: 1  withInteger: 1] != 2)
    {
      abort ();
    }
  if ([TestClass sum: 1  : 1] != 2)
    {
      abort ();
    }
  if ([TestClass sumInteger: 1  withInteger: 1  withInteger: 1] != 3)
    {
      abort ();
    }
  if ([TestClass sum: 1  : 1  : 1] != 3)
    {
      abort ();
    }

  return 0;
}
