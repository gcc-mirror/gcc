/* { dg-do compile { target { loongarch64*-*-* } } } */
/* { dg-options "-O3" } */
/* { dg-final { scan-assembler-not "\tlu12i.w" } } */
/* { dg-final { scan-assembler-not "\tori" } } */
/* { dg-final { scan-assembler-not "\tlu52i.d" } } */
/* { dg-final { scan-assembler-not "\tand" } } */
/* { dg-final { scan-assembler "\tbstrpick.d" } } */
/* { dg-final { scan-assembler "\tbstrins.d" } } */

long
test (long a)
{
  return a & 0x3fffffffefffffff;
}
