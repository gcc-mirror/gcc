/* { dg-lto-do link } */
/* { dg-lto-options { { -O2 -flto -fopenmp-simd } } } */

float b[3][3];

__attribute__((used, noinline)) void
foo ()
{
  int v1, v2;
#pragma omp simd collapse(2)
  for (v1 = 0; v1 < 3; v1++)
    for (v2 = 0; v2 < 3; v2++)
      b[v1][v2] = 2.5;
}

int
main ()
{
  asm volatile ("" : : "g" (b) : "memory");
  foo ();
  asm volatile ("" : : "g" (b) : "memory");
  return 0;
}
