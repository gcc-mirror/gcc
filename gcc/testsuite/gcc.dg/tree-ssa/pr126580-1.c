/* { dg-do compile } */
/* { dg-options "-O2 -ftree-cselim -fdump-tree-phiopt1-details" } */

int *sink(int*);
void f(int a, int c, int d, int *e)
{
  e = sink(&a);
  a = d;
  c = *e;
  c += a;
  if (c)
    a = d|c;
  sink(&a);
}

/* { dg-final { scan-tree-dump "Conditional store replacement" "phiopt1" } } */
