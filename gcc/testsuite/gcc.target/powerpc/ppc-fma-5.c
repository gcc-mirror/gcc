/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-options "-O2 -mcpu=power5 -std=c99" } */

#ifndef __FP_FAST_FMA
#error "__FP_FAST_FMA should be defined"
#endif

#ifndef __FP_FAST_FMAF
#error "__FP_FAST_FMAF should be defined"
#endif

double d_a = 2.0,  d_b = 3.0,  d_c = 4.0;
float  f_a = 2.0f, f_b = 3.0f, f_c = 4.0f;

int
main (void)
{
  if (__builtin_fma (d_a, d_b, d_c) != (2.0 * 3.0) + 4.0)
    __builtin_abort ();

  if (__builtin_fmaf (f_a, f_b, f_c) != (2.0f * 3.0f) + 4.0f)
    __builtin_abort ();

  return 0;
}
