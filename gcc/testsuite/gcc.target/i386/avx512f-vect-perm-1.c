/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math -march=skylake-avx512 -mprefer-vector-width=512" } */
/* { dg-final { scan-assembler-times "vpermd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */

#define N 1024
float f1[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
float f2[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));

void foo ()
{
  int j;
  for (j=0; j<N; j++)
    f1[j] += f2[N-j];
}
