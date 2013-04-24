/* { dg-do compile } */
/* { dg-options "-fopenmp" } */
/* { dg-additional-options "-std=c99" { target c } } */

void baz (void) __attribute__((noreturn));

void
foo (int x)
{
  if (x)
  #pragma omp simd
    for (int i = 0; i < 10; i++)
      baz ();
#pragma omp simd collapse(3)
  for (int i = 0; i < 10; i++)
    for (int j = 0; j < 10; j++)
      for (int k = 0; k < 10; k++)
	baz ();
}
