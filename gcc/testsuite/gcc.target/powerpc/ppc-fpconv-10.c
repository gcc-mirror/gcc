/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mcpu=power7 -ffast-math" } */
/* { dg-final { scan-assembler "xsrdpiz" } } */
/* { dg-final { scan-assembler-not "friz" } } */

double round_double_llong (double a)
{
  return (double)(long long)a;
}
