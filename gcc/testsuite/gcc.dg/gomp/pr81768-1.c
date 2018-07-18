/* PR middle-end/81768 */
/* { dg-do compile } */

float b[10][15][10];

void
foo (void)
{
  float *i;
#pragma omp target parallel for simd schedule(static, 32) collapse(3)
  for (i = &b[0][0][0]; i < &b[0][0][10]; i++)
    for (float *j = &b[0][15][0]; j > &b[0][0][0]; j -= 10)
      for (float *k = &b[0][0][10]; k > &b[0][0][0]; --k)
	b[i - &b[0][0][0]][(j - &b[0][0][0]) / 10 - 1][(k - &b[0][0][0]) - 1] -= 3.5;
}
