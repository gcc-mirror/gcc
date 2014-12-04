/* { dg-do compile } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power6" } } */
/* { dg-options "-O2 -ffast-math -mcpu=power6" } */

double foo (double x, double y)
{
  return __builtin_pow (x, 0.75) + y;
}


/* { dg-final { scan-assembler "fmadd" { target powerpc*-*-* } } } */
