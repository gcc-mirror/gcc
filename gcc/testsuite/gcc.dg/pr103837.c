/* PR rtl-optimization/103837 */
/* { dg-do compile } */
/* { dg-options "-Og -fcompare-debug -fmove-loop-invariants -fnon-call-exceptions -fexceptions -fdelete-dead-exceptions -fno-tree-dce -w" } */

unsigned long int
foo (int x)
{
  double a;
  int b;
  unsigned long int ret = a;

  for (;;)
    {
      b = !!((int) a);
      a = x;
    }

  return ret;
}
