/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-skip-if "" { powerpc*-*-*spe* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mcpu=power7 -O2" } */
/* { dg-final { scan-assembler-times "cdtbcd " 1 } } */
/* { dg-final { scan-assembler-times "cbcdtd " 1 } } */
/* { dg-final { scan-assembler-times "addg6s " 1 } } */
/* { dg-final { scan-assembler-not    "bl __builtin" } } */

unsigned int
to_bcd (unsigned int a)
{
  return __builtin_cdtbcd (a);
}

unsigned int
from_bcd (unsigned int a)
{
  return __builtin_cbcdtd (a);
}

unsigned int
bcd_arith (unsigned int a, unsigned int b)
{
  return __builtin_addg6s (a, b);
}
