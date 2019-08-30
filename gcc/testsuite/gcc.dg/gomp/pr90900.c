/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp -g" } */

void f (int a)
{
  void *x = &&lab;
#pragma omp parallel
  if (a)
    { lab: __builtin_unreachable(); }
  x;
}
