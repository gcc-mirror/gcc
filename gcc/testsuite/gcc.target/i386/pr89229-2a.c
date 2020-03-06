/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -march=skylake-avx512" } */

typedef __int128 __m128t __attribute__ ((__vector_size__ (16),
					 __may_alias__));

__m128t
foo1 (void)
{
  register __int128 xmm16 __asm ("xmm16") = (__int128) -1;
  asm volatile ("" : "+v" (xmm16));
  return (__m128t) xmm16;
}

/* { dg-final { scan-assembler-not "%zmm\[0-9\]+" } } */
