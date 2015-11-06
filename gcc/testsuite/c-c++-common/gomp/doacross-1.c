/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

void
foo (void)
{
  int i, j, k;
  #pragma omp for ordered (1)
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered depend (sink: i - 1)
      #pragma omp ordered depend (source)
    }
  #pragma omp for ordered (1) collapse (1)
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered depend (sink: i - 1)
      #pragma omp ordered depend (source)
    }
  #pragma omp for collapse (2) ordered (1)		/* { dg-error "clause parameter is less than" } */
  for (i = 0; i < 64; i++)
    for (j = 0; j < 64; j++)
      {
	#pragma omp ordered depend (sink: i - 1)	/* { dg-error "does not match number" } */
	#pragma omp ordered depend (source)
      }
  #pragma omp for ordered (2) collapse (3)		/* { dg-error "clause parameter is less than" } */
  for (i = 0; i < 64; i++)
    for (j = 0; j < 64; j++)
      for (k = 0; k < 64; k++)
	{
	  #pragma omp ordered depend (sink: i - 1, j - 2) /* { dg-error "does not match number" } */
	  #pragma omp ordered depend (source)
	}
  #pragma omp ordered depend (sink: j)			/* { dg-error "clause must be closely nested inside an .ordered. loop" } */
  #pragma omp ordered depend (source)			/* { dg-error "clause must be closely nested inside an .ordered. loop" } */
  #pragma omp for ordered (1)
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered depend (sink: i - 1) depend (sink: i - 2)
      #pragma omp ordered depend (source) depend (source) /* { dg-error "more than one .depend.source.. clause on an" } */
    }
  #pragma omp for ordered (1)
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered depend (sink: i - 1) depend (source) depend (sink: i - 2) /* { dg-error "clause specified together with" } */
    }
}
