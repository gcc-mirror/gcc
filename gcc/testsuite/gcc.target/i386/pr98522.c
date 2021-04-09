/* PR target/98522 */
/* { dg-do run { target { ! ia32 } } } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target fenv_exceptions } */

#include <emmintrin.h>
#include <fenv.h>

__m64
__attribute__((noinline))
test_cvt (__m128 a)
{
  return _mm_cvt_ps2pi (a);
}

__m64
__attribute__((noinline))
test_cvtt (__m128 a)
{
  return _mm_cvtt_ps2pi (a);
}

int
main ()
{
  __m128 x = (__m128)(__m128i){0x0000000000000000LL, 0x7fffffffffffffffLL};
  volatile __m64 y;

  feclearexcept (FE_INVALID);

  y = test_cvt(x);
  y = test_cvtt (x);

    if (fetestexcept (FE_INVALID))
    __builtin_abort ();

  return 0;
}

