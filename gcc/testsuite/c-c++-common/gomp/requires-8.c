#pragma omp requires atomic_default_mem_order(acquire)

int
bar (int a, int b)
{
  int c;

  #pragma omp atomic write /* { dg-error "'#pragma omp atomic write' incompatible with 'acquire' clause implicitly provided by a 'requires' directive" } */
    a = b;

  #pragma omp atomic read
    c = a;
  return c;
}
