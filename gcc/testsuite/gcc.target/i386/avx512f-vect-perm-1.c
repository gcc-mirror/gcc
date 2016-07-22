/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math -march=knl" } */
/* { dg-final { scan-assembler-times "vpermps\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */

#define N 1024
float f1[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
float f2[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));

void foo ()
{
  int j;
  for (j=0; j<N; j++)
    f1[j] += f2[N-j];
}
