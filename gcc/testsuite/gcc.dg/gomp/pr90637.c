/* PR tree-optimization/90637 */
/* { dg-do compile } */
/* { dg-options "-fopenmp -O1 --param sink-frequency-threshold=90" } */

int v;

void
foo (int c)
{
  int i;
#pragma omp for simd if (c) lastprivate (v) schedule (static, 16)
  for (i = 0; i < 64; ++i)
    v = i;
}
