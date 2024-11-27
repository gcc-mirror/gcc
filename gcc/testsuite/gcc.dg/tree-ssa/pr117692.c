/* PR tree-optimization/117692 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */
/* { dg-final { scan-tree-dump " \\\* 25;" "vrp1" } } */
/* { dg-final { scan-tree-dump " \\\+ 800;" "vrp1" } } */
/* { dg-final { scan-tree-dump " = \\\(unsigned int\\\) " "vrp1" } } */
/* { dg-final { scan-tree-dump " = \\\(int\\\) " "vrp1" } } */

int
foo (int x)
{
  if (x & 7)
    __builtin_unreachable ();
  x /= 8;
  x += 4;
  return x * 200;
}
