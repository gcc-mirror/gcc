/* PR target/128229 */
/* { dg-do run } */
/* { dg-options "-O2 -msse2 -mfpmath=sse -ftrapping-math" } */
/* { dg-require-effective-target fenv_exceptions } */
/* { dg-require-effective-target sse2_runtime } */

#include <emmintrin.h>
#include <fenv.h>

/* minpd (UNSPEC_IEEE_MIN) raises FE_INVALID on a NaN operand.  It is loop
   invariant here, but it must not be hoisted above the guard.  */

__attribute__((noipa)) __m128d
foo (__m128d a, __m128d b, int *p, int n)
{
  __m128d r = _mm_setzero_pd ();
  for (int i = 0; i < n; i++)
    if (p[i])
      r = _mm_add_pd (r, _mm_min_pd (a, b));
  return r;
}

int p[64];

int
main (void)
{
  __m128d a = _mm_set1_pd (__builtin_nan (""));
  __m128d b = _mm_set1_pd (1.0);
  double r[2];

  feclearexcept (FE_INVALID);
  _mm_storeu_pd (r, foo (a, b, p, 64));
  if (r[0] != 0.0 || r[1] != 0.0)
    __builtin_abort ();
  if (fetestexcept (FE_INVALID))
    __builtin_abort ();
  return 0;
}
