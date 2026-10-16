/* { dg-do run } */

#include <limits.h>

__attribute__((noipa)) int
s_sat (int x)
{
  int sum = (int) ((unsigned) x + 1u);
  if (((x ^ sum) & (x ^ 2)) < 0)
    return -(int) (x < 0) ^ INT_MAX;
  return sum;
}

__attribute__((noipa)) signed char
another_sat (int a, int b)
{
  signed char r;
  if (__builtin_add_overflow (a, b, &r))
    return a < 0 ? SCHAR_MIN : SCHAR_MAX;
  return r;
}

int main (void)
{
  if (s_sat (INT_MAX) != INT_MIN)
    __builtin_abort ();

  if (s_sat (-1) != INT_MIN)
    __builtin_abort ();

  if (another_sat (-1, 200) != -128)
    __builtin_abort ();

  if (another_sat (100, 100) != 127)
    __builtin_abort ();

  __builtin_exit (0);
}
