/* { dg-do compile } */
/* { dg-options "-march=rv32gc -mabi=ilp32 -O2 -mtune=xt-c908" } */

double
foo (int c, double x, double y)
{
  return c ? x : y;
}

/* { dg-final { scan-assembler-not {\mfld\M} } } */
/* { dg-final { scan-assembler-not {\mfsd\M} } } */
