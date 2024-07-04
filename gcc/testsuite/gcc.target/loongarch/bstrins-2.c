/* { dg-do compile } */
/* { dg-options "-O2 -march=loongarch64 -mabi=lp64d" } */
/* { dg-final { scan-assembler "bstrins\\.d\t\\\$r\[0-9\]+,\\\$r0,4,0" } } */

struct aligned_buffer {
  _Alignas(32) char x[1024];
};

extern int f(char *);
int g(void)
{
  struct aligned_buffer buf;
  return f(buf.x);
}
