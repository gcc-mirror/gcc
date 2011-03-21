/* PR tree-optimization/47060 */
/* { dg-do compile } */
/* { dg-options "-O -ffast-math -ftree-parallelize-loops=2 -fno-tree-dce" } */

struct S
{
  int n;
  float *a;
};

float
foo (struct S *b)
{
  float c, d;
  int j;
  for (j = 0; j < b->n; j++)
    d += b->a[j];
  for (j = 0; j < b->n; j++)
    c += b->a[j];
  return d;
}
