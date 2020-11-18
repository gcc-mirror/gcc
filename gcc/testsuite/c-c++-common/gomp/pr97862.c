/* PR middle-end/97862 */

void
foo (void)
{
  int i, j;
#pragma omp for collapse(2)
  for (i = 0; i < 1; ++i)
    for (j = 0; j < i; ++j)
      ;
#pragma omp for collapse(2)
  for (i = 0; i < 20; i++)
    for (j = 0; j < i - 19; j += 1)
      ;
}
