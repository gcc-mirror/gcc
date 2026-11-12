/* { dg-do run } */
/* { dg-require-effective-target fenv_exceptions } */
/* { dg-additional-options "-ftrapping-math" } */

#include <fenv.h>

volatile int sink;
int a[16];
int b[16];

__attribute__((noipa)) void
f (double x, int n)
{
  for (int i = 0; i < n; i++)
    if (a[i])
      {
	if (x < 1.0)
	  sink = 1;
	else
	  sink = 2;
      }
}

__attribute__((noipa)) void
g (double x, int n, int m)
{
  for (int j = 0; j < m; j++)
    if (b[j])
      for (int i = 0; i < n; i++)
	if (x < 1.0)
	  sink = 1;
	else
	  sink = i;
}

int
main (void)
{
  volatile double nan = __builtin_nan ("");
  feclearexcept (FE_INVALID);
  f (nan, 16);
  if (fetestexcept (FE_INVALID))
    __builtin_abort ();
  g (nan, 16, 16);
  if (fetestexcept (FE_INVALID))
    __builtin_abort ();
  return 0;
}
