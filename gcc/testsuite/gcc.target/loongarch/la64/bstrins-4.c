/* { dg-do compile } */
/* { dg-options "-O2 -march=loongarch64 -mabi=lp64d" } */
/* { dg-final { scan-assembler "bstrins\\.d\t\\\$r4,\\\$r0,2,2" } } */

long
x (long a)
{
  return a & ~4;
}
