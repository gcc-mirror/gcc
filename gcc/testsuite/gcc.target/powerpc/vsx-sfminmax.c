/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-O2 -mdejagnu-cpu=power7 -ffast-math" } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-final { scan-assembler "xsmaxdp" } } */
/* { dg-final { scan-assembler "xsmindp" } } */

float
do_fmin (float a, float b)
{
  return __builtin_fminf (a, b);
}

float
do_fmax (float a, float b)
{
  return __builtin_fmaxf (a, b);
}
