/* PR middle-end/66429 */
/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp" } */

float b[10][15][10];

__attribute__ ((noreturn)) void
noreturn (void)
{
  for (;;);
}

__attribute__ ((noinline, noclone)) void
foo (int n)
{
  int i;

#pragma omp parallel for simd schedule(static, 32) collapse(3)
  for (i = 0; i < 10; i++)
    for (int j = n; j < 8; j++)
      for (long k = -10; k < 10; k++)
	{
	  b[i][j][k] += 16;
	  noreturn ();
	  b[i][j][k] -= 32;
	}
}

__attribute__ ((noinline, noclone)) void
bar (void)
{
  int i;

#pragma omp parallel for simd schedule(static, 32)
  for (i = 0; i < 10; i++)
    {
      b[0][0][i] += 16;
      noreturn ();
      b[0][0][i] -= 32;
    }
}
