/* PR middle-end/91216 */

int r;

void
foo (int *a)
{
  int i;
  #pragma omp for reduction(+:r)
  for (i = 0; i < 64; i++)
    a[i] = i;
  #pragma omp for private (r)
  for (i = 0; i < 64; i++)
    {
      r = 0;
      #pragma omp parallel shared(r)
      #pragma omp master
      r = r + 1;
    }
}
