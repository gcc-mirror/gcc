/* PR tree-optimization/96480 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -fno-bit-tests -fno-jump-tables" } */
/* { dg-final { scan-tree-dump " = _\[0-9]* <= 3;" "optimized" } } */

int v[4];

int
foo (int x)
{
  int *p;
  if (x == 0)
    p = &v[0];
  else if (x == 1)
    p = &v[1];
  else if (x == 2)
    p = &v[2];
  else if (x == 3)
    p = &v[3];
  else
    p = &v[4];
  return p != &v[4];
}
