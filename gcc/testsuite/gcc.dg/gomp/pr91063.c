/* PR tree-optimization/91063 */
/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp-simd" } */
/* { dg-additional-options "-mavx512f" { target { i?86-*-* x86_64-*-* } } } */

struct S { void *s; };

int
foo (struct S *x)
{
  int r = 0;
  int i;
#pragma omp simd reduction (+ : r)
  for (i = 0; i < 64; ++i)
    r += (int) (x->s != 0);
  return r;
}
