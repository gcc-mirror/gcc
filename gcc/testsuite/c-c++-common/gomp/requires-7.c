#pragma omp requires atomic_default_mem_order(release)

int
foo (int x)
{
  int z;

  #pragma omp atomic read /* { dg-error "'#pragma omp atomic read' incompatible with 'release' clause implicitly provided by a 'requires' directive" } */
    z = x;
  return z;
}
