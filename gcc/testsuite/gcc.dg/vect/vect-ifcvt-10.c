/* PR 21272 */
/* { dg-do compile } */
double
foo (int j, double *v, double x)
{
  int i;
  for (i = 0; i < j; i++)
    if (v[i] < x)
      x = v[i];
  return x;
}
/* { dg-final { cleanup-tree-dump "vect" } } */
