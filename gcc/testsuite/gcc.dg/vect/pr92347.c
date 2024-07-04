/* Copied from PR 92347.  */
/* { dg-do compile } */
/* { dg-additional-options "-O1 -fopenmp-simd" } */
/* { dg-additional-options "-mavx" { target { i?86-*-* x86_64-*-* } } } */

int by;

#pragma omp declare simd
int
zp (int);

void
qh (int oh)
{
#pragma omp simd
  for (by = 0; by < oh; ++by)
    by = zp (by);
}
