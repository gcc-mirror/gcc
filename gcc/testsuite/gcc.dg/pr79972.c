/* PR tree-optimization/79972 */
/* { dg-do compile } */
/* { dg-require-effective-target alloca } */
/* { dg-options "-Walloca -Wvla-larger-than=10000" } */

int
f (int dim, int *b, int *c)
{
  int newcentroid[3][dim];	/* { dg-warning "unbounded use of variable-length array" } */
  int *a = newcentroid[2];
  int i, dist = 0;
  __builtin_memcpy (newcentroid, c, sizeof (newcentroid));
  for (i = 0; i < dim; i++)
    dist += (a[i] - b[i]) * (a[i] - b[i]);
  return dist;
}
