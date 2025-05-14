/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

int fa(int a, int b)
{
  int c = a * b;
  if (c != 0)
    return (a != 0);
  return 0;
}
int fb(int a, int b)
{
  int c = a * b;
  if (c != 0)
    return (b != 0);
  return 0;
}

/* { dg-final { scan-tree-dump-times "PHI <1" 2 "evrp" } } */
