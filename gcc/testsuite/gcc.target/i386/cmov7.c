/* PR middle-end/33187 */

/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-march=*" } { "-march=k8" } } */
/* { dg-options "-O2 -ffast-math -march=k8 -mbranch-cost=5 -mfpmath=387" } */
/* { dg-final { scan-assembler "fcmov" } } */

/* compress_float_constant generates load + float_extend
   sequence which combine pass failed to combine into
   (set (reg:DF) (float_extend:DF (mem:SF (symbol_ref...)))).  */

double
foo (double __x)
{
  return __x >= 1.0 ? 0.0 : -1.0;
}
