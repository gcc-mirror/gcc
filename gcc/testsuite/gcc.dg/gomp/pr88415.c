/* { dg-do compile } */
/* { dg-options "-fexceptions -fnon-call-exceptions -fopenmp -fsignaling-nans -funsafe-math-optimizations -fno-associative-math" } */

void
lx (_Complex int *yn)
{
  int mj;

#pragma omp for
  for (mj = 0; mj < 1; ++mj)
    yn[mj] += 1;
}
