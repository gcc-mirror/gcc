/* { dg-do compile } */
/* { dg-options "-O2 -march=loongarch64 -mabi=lp64d" } */
/* { dg-final { scan-assembler "foo:.*\tbstrins\\.d\t\\\$r\[0-9\]+,\\\$r\[0-9\]+,63,1.*foo" } } */
/* { dg-final { scan-assembler "bar:.*\tbstrins\\.d\t\\\$r\[0-9\]+,\\\$r\[0-9\]+,63,15.*bar" } } */

long foo (long a, long b)
{
  return (a << 1) | (b & 1);
}

short bar (short a, short b)
{
  return (a << 15) | (b & 0x7fff);
}
