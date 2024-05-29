/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-O2 -mdejagnu-cpu=power7 -ffast-math" } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-final { scan-assembler {\mfriz\M|\mxsrdpiz\M} } } */

double round_double_llong (double a)
{
  return (double)(long long)a;
}
