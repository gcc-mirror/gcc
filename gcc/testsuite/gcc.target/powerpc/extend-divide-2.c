/* { dg-do compile { target { powerpc*-*-linux* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-skip-if "" { powerpc*-*-*spe* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mcpu=power7 -O2" } */
/* { dg-final { scan-assembler-times "divde "   1 } } */
/* { dg-final { scan-assembler-times "divdeo "  1 } } */
/* { dg-final { scan-assembler-times "divdeu "  1 } } */
/* { dg-final { scan-assembler-times "divdeuo " 1 } } */
/* { dg-final { scan-assembler-not    "bl __builtin" } } */

long
div_de (long a, long b)
{
  return __builtin_divde (a, b);
}

long
div_deo (long a, long b)
{
  return __builtin_divdeo (a, b);
}

unsigned long
div_deu (unsigned long a, unsigned long b)
{
  return __builtin_divdeu (a, b);
}

unsigned long
div_deuo (unsigned long a, unsigned long b)
{
  return __builtin_divdeuo (a, b);
}
