/* PR target/79733 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */

typedef unsigned short __mmask16;

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_kortestc (__mmask16 __A, __mmask16 __B)
{
  return (__mmask16) __builtin_ia32_kortestchi ((__mmask16) __A,
                                                (__mmask16) __B);
}

void
avx512f_test ()
{
  volatile __mmask16 k1 = 0;
  __mmask16 k2 = 0;
  volatile short r;

  r = _mm512_kortestc (k1, k2);
}
