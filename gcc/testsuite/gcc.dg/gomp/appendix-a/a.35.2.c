/* { dg-do compile } */

void work (int, int);

void
work1 (int i, int n)
{
  int j;
  /* incorrect nesting of loop regions */
#pragma omp for
  for (j = 0; j < n; j++)
    work (i, j);
}

void
wrong2 (int n)
{
#pragma omp parallel default(shared)
  {
    int i;
#pragma omp for
    for (i = 0; i < n; i++)
      work1 (i, n);
  }
}
