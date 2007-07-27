/* { dg-do run } */

#include <limits.h>
extern void abort (void);

int f(unsigned int a, unsigned int b)
{
  if (a > INT_MAX - 15) return 0;
  if (b > INT_MAX - 15) return 0;

  int c = (a - 20) + (b - 20);
  return c > INT_MAX - 15;
}

int main()
{
  if (f (INT_MAX - 15, 41) != 1)
    abort ();
  return 0;
}
