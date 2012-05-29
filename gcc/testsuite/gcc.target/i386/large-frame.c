/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-final { scan-assembler "-429496" } } */
extern void dump (int *buf, int a);

void func (int a)
{
  int bigbuf[1 << 30];
  dump (bigbuf, a);
}
