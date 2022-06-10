/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

int i;

#pragma omp declare simd linear (x : val, step (1)) linear (y : step (2))
int bar (int x, int y, int z);
#pragma omp declare simd linear (x : step (1), val)
int baz (int x, int y, int z);
#pragma omp declare simd linear (val (x) : val) uniform (val)
int qux (int x, int val);
#pragma omp declare simd linear (x : val, step (val)) uniform (val)
int corge (int x, int val);
#pragma omp declare simd linear (x : val)
int grault (int x);
int step (int);

void
foo (int x, int y)
{
  int val = 1;
  #pragma omp simd linear (i: step (3))
  for (i = 0; i < 33; i += 3)
    ;
  #pragma omp simd linear (i: val, step (3))
  for (i = 0; i < 33; i += 3)
    ;
  #pragma omp simd linear (x: step (y + 1))
  for (i = 0; i < 10; i++)
    x += y + 1;
  #pragma omp simd linear (x: step (y + 1), val)
  for (i = 0; i < 10; i++)
    x += y + 1;
  #pragma omp parallel for linear (x: step (y + 1))
  for (i = 0; i < 10; i++)
    x += y + 1;
  #pragma omp parallel for linear (x: val, step (y + 1))
  for (i = 0; i < 10; i++)
    x += y + 1;
  #pragma omp parallel for simd linear (i: step (3))
  for (i = 0; i < 33; i += 3)
    ;
  #pragma omp parallel for simd linear (i: step (3), val)
  for (i = 0; i < 33; i += 3)
    ;
  #pragma omp parallel for simd linear (x: step (y + 1))
  for (i = 0; i < 10; i++)
    x += y + 1;
  #pragma omp parallel for simd linear (x: val, step (y + 1))
  for (i = 0; i < 10; i++)
    x += y + 1;
  #pragma omp parallel for simd linear (i: val + 0)
  for (i = 0; i < 10; i++)
    ;
  #pragma omp parallel for simd linear (i: step (1) * 1)
  for (i = 0; i < 10; i++)
    ;
}
