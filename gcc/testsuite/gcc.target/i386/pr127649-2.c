/* PR target/127649 */
/* { dg-do run } */
/* { dg-options "-O2 -msse2 -mno-avx" } */
/* { dg-require-effective-target fenv_exceptions } */
/* { dg-additional-options "-mfpmath=sse" { target ia32 } } */

#include <fenv.h>
#include "pr127649-1.c"

#define N 1024

static float a[N], b[N];
static int r[N];

static void
check (void (*fn) (int *, float *, float *))
{
  for (int i = 0; i < N; i++)
    r[i] = -1;
  feclearexcept (FE_ALL_EXCEPT);
  fn (r, a, b);
  if (!fetestexcept (FE_INVALID))
    __builtin_abort ();
  for (int i = 0; i < N; i++)
    if (r[i] != 0)
      __builtin_abort ();
}

int
main (void)
{
  __builtin_cpu_init ();

  for (int i = 0; i < N; i++)
    {
      a[i] = __builtin_nanf ("");
      b[i] = 1.0f;
    }

  check (ltgt_sse);
  if (__builtin_cpu_supports ("avx"))
    check (ltgt_avx);
  if (__builtin_cpu_supports ("avx512vl"))
    check (ltgt_avx512);
  return 0;
}
