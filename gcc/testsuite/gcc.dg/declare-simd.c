/* { dg-do compile } */
/* { dg-options "-O3 -fopenmp-simd" } */

#pragma omp declare simd linear (p2, p3)
extern void fn2 (float p1, float *p2, float *p3);

float *a, *b;
void fn1 (float *p1)
{
  int i;
#pragma omp simd
  for (i = 0; i < 1000; i++)
    fn2 (p1[i], a + i, b + i);
}
