/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

int f(int i)
{
  int t = i == 1;
  int g = t == 2;
  int h = g == 3;
  return h;
}

/* { dg-final { scan-tree-dump "return 0;" "vrp1" } } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */

