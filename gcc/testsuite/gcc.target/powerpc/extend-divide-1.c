/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-skip-if "" { powerpc*-*-*spe* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power7 -O2" } */
/* { dg-final { scan-assembler-times "divwe "   1 } } */
/* { dg-final { scan-assembler-times "divweu "  1 } } */
/* { dg-final { scan-assembler-not    "bl __builtin" } } */

int
div_we (int a, int b)
{
  return __builtin_divwe (a, b);
}

unsigned int
div_weu (unsigned int a, unsigned int b)
{
  return __builtin_divweu (a, b);
}
