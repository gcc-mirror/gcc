/* PR middle-end/87649 */

void
foo (void)
{
  int i;
  #pragma omp for ordered(1)
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered			/* { dg-error "'ordered' region without 'depend' clause may not be closely nested inside a loop region with an 'ordered' clause with a parameter" } */
      ;
    }
  #pragma omp for ordered(1)
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered threads		/* { dg-error "'ordered' region without 'depend' clause may not be closely nested inside a loop region with an 'ordered' clause with a parameter" } */
      ;
    }
}

void
bar (void)
{
  int i;
  #pragma omp for ordered
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered depend(source)	/* { dg-error "'ordered' construct with 'depend' clause must be closely nested inside a loop with 'ordered' clause with a parameter" } */
      #pragma omp ordered depend(sink: i - 1)	/* { dg-error "'ordered' construct with 'depend' clause must be closely nested inside a loop with 'ordered' clause with a parameter" } */
    }
  #pragma omp for
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered depend(source)	/* { dg-error "'ordered' construct with 'depend' clause must be closely nested inside a loop with 'ordered' clause with a parameter" } */
      #pragma omp ordered depend(sink: i - 1)	/* { dg-error "'ordered' construct with 'depend' clause must be closely nested inside a loop with 'ordered' clause with a parameter" } */
    }
  #pragma omp for
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered			/* { dg-error "'ordered' region must be closely nested inside a loop region with an 'ordered' clause" } */
      ;
    }
  #pragma omp for
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered threads		/* { dg-error "'ordered' region must be closely nested inside a loop region with an 'ordered' clause" } */
      ;
    }
}
