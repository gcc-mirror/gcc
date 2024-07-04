/* { dg-do compile { target { powerpc*-*-linux* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-mdejagnu-cpu=power7 -O2" } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-final { scan-assembler-times "divde "   1 } } */
/* { dg-final { scan-assembler-times "divdeu "  1 } } */
/* { dg-final { scan-assembler-not    "bl __builtin" } } */

long
div_de (long a, long b)
{
  return __builtin_divde (a, b);
}

unsigned long
div_deu (unsigned long a, unsigned long b)
{
  return __builtin_divdeu (a, b);
}
