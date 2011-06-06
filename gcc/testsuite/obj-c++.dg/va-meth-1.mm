/* Based on objc/execute/va_method.m, by Nicola Pero */

/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
#include "../objc-obj-c++-shared/TestsuiteObject.m"
#include <stdarg.h>
#include <stdlib.h>

/* Test methods with "C-style" trailing arguments, with or without ellipsis. */

@interface MathClass: TestsuiteObject
/* sum positive numbers; -1 ends the list */
+ (int) sum: (int) firstNumber, int secondNumber, ...;
+ (int) prod: (int) firstNumber, int secondNumber, int thirdNumber;
+ (int) minimum: (int) firstNumber, ...;
@end

extern "C" int some_func(id self, SEL _cmd, int firstN, int secondN, int thirdN, ...) {
  return firstN + secondN + thirdN;
}

@implementation MathClass
+ (int) sum: (int) firstNumber, int secondNumber, ...
{
  va_list ap;
  int sum = 0, number = 0;

  va_start (ap, secondNumber);
  number = firstNumber + secondNumber;

  while (number >= 0)
    {
      sum += number;
      number = va_arg (ap, int);
    }
  
  va_end (ap);

  return sum;
}
+ (int) prod: (int) firstNumber, int secondNumber, int thirdNumber {
  return firstNumber * secondNumber * thirdNumber;
}
+ (int) minimum: (int) firstNumber, ...
{
  va_list ap;
  int minimum = 999, number = 0;
  
  va_start (ap, firstNumber);
  number = firstNumber;
  
  while (number >= 0)
    {
      minimum = (minimum < number ? minimum: number);
      number = va_arg (ap, int);
    }
  
  va_end (ap);
  
  return minimum;
}
@end

int main (void)
{
  if ([MathClass sum: 1, 2, 3, 4, 5, -1] != 15)
    abort ();
  if ([MathClass prod: 4, 5, 6] != 120)
    abort ();
  if ([MathClass minimum: 17, 9, 133, 84, 35, -1] != 9)
    abort ();
  
  return 0;
}

