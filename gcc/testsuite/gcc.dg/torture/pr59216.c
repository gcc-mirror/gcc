/* { dg-do run } */

#include <limits.h>

extern void abort (void);
extern void exit (int);

long long __attribute__((noinline)) f(int a)
{
  return -(long long) a;
}

int
main()
{
  if (f(0) != 0)
    abort ();

  if (f(1) != -(long long)1)
    abort ();

  if (f(-1) != -(long long)-1)
    abort ();

  if (f(INT_MIN) != -(long long)INT_MIN)
    abort ();

  if (f(INT_MAX) != -(long long)INT_MAX)
    abort ();

  exit (0);
}
