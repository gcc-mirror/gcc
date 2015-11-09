/* { dg-do compile } */
/* { dg-options "-O" } */
/* { dg-final { scan-assembler "[fg]s:0" } } */

void test(int *y)
{
  int *x = (int __seg_tls *)0;
  if (x == y)
    asm("");
}
