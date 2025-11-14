/* { dg-do compile } */
/* { dg-options "-O2 -march=loongarch64 -mabi=lp64d" } */
/* { dg-final { scan-assembler "foo:.*\tbstrins\\.w\t\\\$r\[0-9\]+,\\\$r\[0-9\]+,31,1.*foo" } } */
/* { dg-final { scan-assembler "bar:.*\tbstrins\\.w\t\\\$r\[0-9\]+,\\\$r\[0-9\]+,31,31.*bar" } } */

int foo (int a, int b)
{
  return (a << 1) | (b & 1);
}

int bar (int a, int b)
{
  return (a << 31) | (b & 0x7fffffff);
}
