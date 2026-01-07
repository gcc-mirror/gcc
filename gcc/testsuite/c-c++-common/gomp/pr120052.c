/* PR middle-end/120052 */
/* { dg-do compile } */
/* { dg-additional-options "-fsanitize=undefined" } */

void
foo (unsigned long s, long indx)
{
  long counts[2][s];
  #pragma omp parallel
  #pragma omp masked
  for (int i = 0; i < 2; i++)
    counts[2][indx] = 1;
}

void
bar (unsigned long s, long indx)
{
  long counts[2][s];
  #pragma omp parallel shared(counts)
  #pragma omp masked
  for (int i = 0; i < 2; i++)
    counts[2][indx] = 1;
}

void
baz (unsigned long s, long indx)
{
  long counts[2][s];
  #pragma omp parallel private(counts)
  for (int i = 0; i < 2; i++)
    counts[2][indx] = 1;
}
