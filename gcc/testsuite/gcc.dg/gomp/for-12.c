int foo (void)
{
  int i, a;

  a = 30;

  #pragma omp parallel for lastprivate (a)
  for (i = 0; i < 10; i++)
    a = a + i;

  return a;
}
