/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_fprs } */
/* { dg-options "-O2 -mdejagnu-cpu=power5+ -ffast-math" } */
/* { dg-final { scan-assembler-not "xsrdpiz" } } */
/* { dg-final { scan-assembler "friz" } } */

double round_double_llong (double a)
{
  return (double)(long long)a;
}
