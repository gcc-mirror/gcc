/* { dg-do run }  */
/* { dg-require-effective-target glibc }  */
/* { dg-options "-O3 -fno-math-errno -ftrapping-math -march=armv8-a+sve" }  */

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>
#include <fenv.h>
#include <signal.h>

#pragma STDC FENV_ACCESS ON

__attribute__((noinline))
void f(float *__restrict c, int n)
{
  for (int i = 0; i < n; i++)
    c[i] = __builtin_sqrtf (c[i] - 2.0f);
}

static void on_fpe (int sig)
{
  (void) sig;
  puts ("SIGFPE: trapped FP exception (unexpected invalid from sqrt)");
  fflush (stdout);
  __builtin_abort ();
}

int
main (void)
{
  signal (SIGFPE, on_fpe);

  /* Clear flags and enable trap on invalid operations.  */
  feclearexcept (FE_ALL_EXCEPT);
  feenableexcept (FE_INVALID);

  /* Choose a length that is NOT a multiple of typical SVE VL (unknown at
     runtime), and includes plenty of extra lanes.  */
  const int n = 37;

  float *c = aligned_alloc (64, (size_t) n * sizeof (float));
  if (!c)
    return 1;

  /* Populate c so that (c[i] - 2) is a perfect square; this avoids FE_INVALID
     while giving deterministic results.  */
  for (int i = 0; i < n; i++)
    {
      int t = i & 3;
      c[i] = (float) (t * t) + 2.0f;
    }

  f (c, n);

  /* Only FE_INVALID would indicate a wrong extra-lane computation here.  */
  if (fetestexcept (FE_INVALID))
    {
      puts ("Unexpected FE_INVALID");
      return 2;
    }

  int ok = 1;
  for (int i = 0; i < n; i++)
    {
      float expected = (float) (i & 3);
      if (!(c[i] == expected))
	{
	  printf ("Mismatch at %d: expected %g, got %g\n", i, expected, c[i]);
	  ok = 0;
	}
    }

  puts (ok ? "OK" : "FAIL");
  free (c);
  return ok ? 0 : 3;
}
