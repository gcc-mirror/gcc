/* PR target/127648 */
/* { dg-do run } */
/* { dg-options "-O3 -mavx512f -ffinite-math-only -save-temps" } */
/* { dg-require-effective-target avx512f } */
/* { dg-final { scan-assembler-times "vminps\[ \\t\]+\{sae\}\[^\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+" 1 } } */

#include "avx512f-check.h"

static unsigned
rd_mxcsr (void)
{
  unsigned m;
  __asm__ __volatile__ ("stmxcsr %0" : "=m" (m) : : "memory");
  return m;
}

static void
wr_mxcsr (unsigned m)
{
  __asm__ __volatile__ ("ldmxcsr %0" : : "m" (m) : "memory");
}

/* -> __builtin_ia32_minps512_mask (a, b, s, -1, 8):  all-ones write mask,
   round argument 8 == _MM_FROUND_NO_EXC, which suppresses all exception
   flags.  -ffinite-math-only must not drop the {sae}.  */
__attribute__((noinline))
static __m512
f (__m512 a, __m512 b, __m512 s)
{
  return _mm512_mask_min_round_ps (s, (__mmask16) -1, a, b, _MM_FROUND_NO_EXC);
}

/* volatile so nothing can constant-propagate the operands into f.  */
static volatile unsigned denorm_bits = 0x00000001u; /* 0x1p-149f: DENORMAL, FINITE */
static volatile float one = 1.0f;
static volatile float zero = 0.0f;

static void
avx512f_test (void)
{
  union { unsigned u; float f; } cv;
  float av[16], bv[16], sv[16];
  __m512 a, b, s, r;
  unsigned m;
  int i;

  cv.u = denorm_bits;
  for (i = 0; i < 16; i++)
    {
      av[i] = cv.f;
      bv[i] = one;
      sv[i] = zero;
    }
  a = _mm512_loadu_ps (av);
  b = _mm512_loadu_ps (bv);
  s = _mm512_loadu_ps (sv);

  wr_mxcsr (0x1f80u);	/* All exceptions masked, all flags clear, FTZ=DAZ=0.  */
  r = f (a, b, s);
  m = rd_mxcsr ();

  /* {sae} must leave the denormal flag (DE, bit 1) clear.  */
  if ((m >> 1) & 1u)
    abort ();

  /* And the value must still be min (0x1p-149f, 1.0f) == 0x1p-149f.  */
  _mm512_storeu_ps (sv, r);
  cv.f = sv[0];
  if (cv.u != 0x00000001u)
    abort ();
}
