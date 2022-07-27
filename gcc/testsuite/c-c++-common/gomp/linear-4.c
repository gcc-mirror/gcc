/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

int step (int x, int y, int z) { return x + y + z; }

int
foo (int x)
{
  int i;
  #pragma omp parallel for linear (x : step (step (1, 2, 3)))
  for (i = 0; i < 64; i++)
    x += 6;
  return x;
}

int
bar (int x)
{
  int i;
  #pragma omp parallel for linear (x : step (1, 2, 3))	/* { dg-error "expected" } */
  for (i = 0; i < 64; i++)
    x += 6;
  return x;
}

int
bar2 (int x)
{
  int i;
  #pragma omp parallel for linear (x : step (1, 2, 3) * 1)
  for (i = 0; i < 64; i++)
    x += 6;
  return x;
}
