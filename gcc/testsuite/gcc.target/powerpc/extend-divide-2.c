/* { dg-do compile { target { powerpc*-*-linux* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-skip-if "" { powerpc*-*-*spe* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-mcpu=power7 -O2" } */
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
