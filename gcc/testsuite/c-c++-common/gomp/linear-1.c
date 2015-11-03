/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

int i;

#pragma omp declare simd linear (val (x) : 1) linear (y : 2)
int bar (int x, int y, int z);

void
foo (int x, int y)
{
  #pragma omp simd linear (i: 3)
  for (i = 0; i < 33; i += 3)
    ;
  #pragma omp simd linear (val (i): 3)		/* { dg-error "modifier should not be specified in" } */
  for (i = 0; i < 33; i += 3)
    ;
  #pragma omp simd linear (x: y + 1)
  for (i = 0; i < 10; i++)
    x += y + 1;
  #pragma omp simd linear (val (x): y + 1)	/* { dg-error "modifier should not be specified in" } */
  for (i = 0; i < 10; i++)
    x += y + 1;
  #pragma omp for linear (x: y + 1)
  for (i = 0; i < 10; i++)
    x += y + 1;
  #pragma omp for linear (val (x): y + 1)	/* { dg-error "modifier should not be specified in" } */
  for (i = 0; i < 10; i++)
    x += y + 1;
  #pragma omp for simd linear (i: 3)
  for (i = 0; i < 33; i += 3)
    ;
  #pragma omp for simd linear (val (i): 3)	/* { dg-error "modifier should not be specified in" } */
  for (i = 0; i < 33; i += 3)
    ;
  #pragma omp for simd linear (x: y + 1)
  for (i = 0; i < 10; i++)
    x += y + 1;
  #pragma omp for simd linear (val (x): y + 1)	/* { dg-error "modifier should not be specified in" } */
  for (i = 0; i < 10; i++)
    x += y + 1;
}
