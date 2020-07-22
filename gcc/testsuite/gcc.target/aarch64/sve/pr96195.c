/* { dg-do compile } */
/* { dg-options "-O1 -fopenmp-simd -ftree-vectorize -msve-vector-bits=128" } */

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

