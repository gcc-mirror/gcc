/* Contributed by Nicola Pero - Thu Mar  8 16:27:46 CET 2001 */
#include <objc/objc.h>
#include <objc/objc-api.h>

/* Test method with variable number of arguments */

@interface MathClass
{
  Class isa;
}
/* sum positive numbers; -1 ends the list */
+ (int) sum: (int)firstNumber, ...;
@end

@implementation MathClass
+ (int) sum: (int)firstNumber, ...
{
  va_list ap;
  int sum = 0, number = 0;

  va_start (ap, firstNumber);
  number = firstNumber;

  while (number >= 0)
    {
      sum += number;
      number = va_arg (ap, int);
    }
  
  va_end (ap);

  return sum;
}
@end

int main (void)
{
  if ([MathClass sum: 1, 2, 3, 4, 5, -1] != 15)
    {
      abort ();
    }
  
  return 0;
}
