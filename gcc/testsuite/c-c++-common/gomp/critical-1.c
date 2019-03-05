int i;

void
foo (void)
{
  #pragma omp critical
  i = i + 1;
  #pragma omp critical (foo)
  i = i + 1;
  #pragma omp critical (foo) hint (0)
  i = i + 1;
  #pragma omp critical (foo),hint(1)
  i = i + 1;
}
