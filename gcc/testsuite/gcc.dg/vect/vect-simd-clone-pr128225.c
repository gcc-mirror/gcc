/* { dg-do compile } */
/* { dg-require-effective-target vect_simd_clones } */
/* { dg-additional-options "-fopenmp-simd" } */
/* { dg-additional-options "-mavx2" { target { x86_64-*-* i?86-*-* } } } */

#pragma omp declare simd simdlen(2) inbranch
extern float clone(double) __attribute__((const));

void use(float *restrict d, const double *restrict a, int n)
{
#pragma omp simd
  for (int i = 0; i < n; i++)
    d[i] = clone(a[i]);
}
