/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -mavx -mabi=ms -dp" } */

typedef float __m256 __attribute__ ((__vector_size__ (32), __may_alias__));

extern __m256 x;

extern __m256 __attribute__ ((sysv_abi)) bar (__m256);

void
foo (void)
{
  bar (x);
}

/* { dg-final { scan-assembler-times "avx_vzeroupper" 1 } } */
/* { dg-final { scan-assembler-times "\\*call_value_rex64_ms_sysv" 1 } } */
