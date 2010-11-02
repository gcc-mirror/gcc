/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O0 -mavx -mabi=ms -mtune=generic -dp" } */

typedef float __m256 __attribute__ ((__vector_size__ (32), __may_alias__));

extern __m256 x;

extern void __attribute__ ((sysv_abi))  bar (__m256);

void
foo (void)
{
  bar (x);
}

/* { dg-final { scan-assembler-not "avx_vzeroupper" } } */
/* { dg-final { scan-assembler-times "\\*call_1_rex64_ms_sysv" 1 } } */
