/* PR middle-end/30494 */
/* { dg-do compile } */

int
foo (int n)
{
  int i;
#pragma omp for
  for (i = 0; i < 6; i++)
    {
      int v[n], w[n * 3 + i];
      v[0] = 1;
      w[0] = 2;
    }
  return 0;
}

int
bar (int n)
{
  int i;
#pragma parallel omp for
  for (i = 0; i < 6; i++)
    {
      int v[n], w[n * 3 + i];
      v[0] = 1;
      w[0] = 2;
    }
  return 0;
}
