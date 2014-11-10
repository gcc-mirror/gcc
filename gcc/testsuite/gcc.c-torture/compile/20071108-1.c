/* PR tree-optimization/33680 */
/* { dg-require-effective-target alloca } */

int
f (int dim, int *b, int *c)
{
  int newcentroid[3][dim];
  int *a = newcentroid[2];
  int i, dist = 0;
  __builtin_memcpy (newcentroid, c, sizeof (newcentroid));
  for (i = 0; i < dim; i++)
    dist += (a[i] - b[i]) * (a[i] - b[i]);
  return dist;
}
