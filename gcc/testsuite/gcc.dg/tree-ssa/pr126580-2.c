/* { dg-do compile } */
/* { dg-options "-O2 -ftree-cselim -fdump-tree-phiopt1-details" } */


void f1(int *a, int c, int d, int *e)
{
  *a = d;
  c = *e;
  c += *a;
  int t = d|c;
  if (c)
    *a = t;
}

/* { dg-final { scan-tree-dump "Conditional store replacement" "phiopt1" } } */
