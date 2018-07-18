/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-O2 -mcpu=power7 -ffast-math" } */
/* { dg-final { scan-assembler {\mfriz\M|\mxsrdpiz\M} } } */

double round_double_llong (double a)
{
  return (double)(long long)a;
}
