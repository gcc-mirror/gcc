/* PR middle-end/54017 */
/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

void
f1 (void)
{
#pragma omp parallel sections
  {
#pragma omp section
    {
      for (;;)
	;
    }
  }
}

int
f2 (void)
{
  int i = 0;
#pragma omp parallel
#pragma omp sections reduction(+:i)
  {
#pragma omp section
    {
      for (;;)
	;
    }
  }
  return i;
}

void
f3 (void)
{
#pragma omp parallel sections
  {
#pragma omp section
    {
      for (;;)
	;
    }
#pragma omp section
    ;
  }
}

int
f4 (void)
{
  int i = 0;
#pragma omp parallel
#pragma omp sections reduction(+:i)
  {
#pragma omp section
    {
      for (;;)
	;
    }
#pragma omp section
    ;
  }
  return i;
}
