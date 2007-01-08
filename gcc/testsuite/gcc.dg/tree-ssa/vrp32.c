/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

int f(int i)
{
  _Bool t = !i;
  int g = !t && i;
  int h = g == 3;
  return h;
}

/* { dg-final { scan-tree-dump "return 0;" "vrp1" } } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */

