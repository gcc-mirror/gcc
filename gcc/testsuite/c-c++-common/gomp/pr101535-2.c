/* PR middle-end/101535 */

void
foo (void)
{
  int a = 1, i;
  #pragma omp target map(tofrom:i)
  #pragma omp for lastprivate(i)
  for (i = 1; i < 2; i++)
    ;
}
