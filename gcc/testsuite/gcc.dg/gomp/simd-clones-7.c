/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-fopenmp -w" } */

int array[1000];

#pragma omp declare simd notinbranch simdlen(4)
void foo (int *a, int b)
{
  a[b] = 555;
}

#pragma omp declare simd notinbranch simdlen(4)
void bar (int *a)
{
  *a = 555;
}
