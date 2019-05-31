/* { dg-additional-options "-fopenmp-simd" } */

#include "tree-vect.h"

int v;

__attribute__((noipa)) int
foo (int *a)
{
  int x = 5;
  #pragma omp simd lastprivate (conditional: x) safelen (1)
  for (int i = 0; i < 128; i++)
    if (a[i])
      x = a[i];
  return x;
}

__attribute__((noipa)) int
bar (int *a, int *b)
{
  int x = 0;
  #pragma omp simd lastprivate (conditional: x, v) if (0)
  for (int i = 16; i < 128; ++i)
    {
      if (a[i])
	x = a[i];
      if (b[i])
	v = b[i] + 10;
    }
  return x;
}

int
main ()
{
  int a[128], b[128], i;
  check_vect ();
  for (i = 0; i < 128; i++)
    {
      a[i] = ((i % 11) == 2) ? i + 10 : 0;
      asm volatile ("" : "+g" (i));
      b[i] = ((i % 13) == 5) ? i * 2 : 0;
    }
  if (foo (a) != 133)
    abort ();
  if (bar (b, a) != 244)
    abort ();
  if (v != 143)
    abort ();
  return 0;
}
