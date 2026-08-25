/* { dg-do compile } */
/* { dg-options "-O2 -ftree-cselim -fdump-tree-phiopt1-details" } */
/* PR tree-optimization/127052 */

void sink(int*);

int f(int b)
{
  int a;
  sink(&a);
  a = b;
  if (a)
    a = 0;
  return a;
}


/* { dg-final { scan-tree-dump "Conditional store replacement" "phiopt1" } } */
