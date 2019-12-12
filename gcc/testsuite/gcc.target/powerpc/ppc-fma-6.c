/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -mdejagnu-cpu=power5 -std=c99 -msoft-float" } */
/* { dg-final { scan-assembler-not "fmadd" } } */
/* { dg-final { scan-assembler-not "xsfmadd" } } */

/* Test whether -msoft-float turns off the macros math.h uses for
   FP_FAST_FMA{,F,L}.  */
#ifdef __FP_FAST_FMA
#error "__FP_FAST_FMA should not be defined"
#endif

#ifdef __FP_FAST_FMAF
#error "__FP_FAST_FMAF should not be defined"
#endif

double
builtin_fma (double b, double c, double d)
{
  return __builtin_fma (b, c, d);			/* bl fma  */
}

float
builtin_fmaf (float b, float c, float d)
{
  return __builtin_fmaf (b, c, -d);			/* bl fmaf */
}
