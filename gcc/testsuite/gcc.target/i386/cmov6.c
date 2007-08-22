/* { dg-do compile } */
/* { dg-options "-O2 -march=k8" } */
/* { dg-final { scan-assembler "cmov\[^6\]" } } */

/* Verify that blocks are converted to conditional moves.  */
extern int bar (int, int);
int foo (int c, int d, int e)
{
  int a, b;

  if (c)
    {
      a = 10;
      b = d;
    }
  else
    {
      a = e;
      b = 20;
    }
  return bar (a, b);
}
