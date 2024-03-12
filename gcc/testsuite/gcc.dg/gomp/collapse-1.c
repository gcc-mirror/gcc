/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

int i, j, k;
extern int foo (void);

void
f1 (void)
{
  #pragma omp for collapse (2)
  for (i = 0; i < 5; i++)	/* { dg-error "not enough nested loops" } */
    ;
  {
    for (j = 0; j < 5; j++)
      ;
  }
}

void
f2 (void)
{
  #pragma omp for collapse (2)
  for (i = 0; i < 5; i++)
    {
      {
	{
	  for (j = 0; j < 5; j++)
	    {
	    }
	}
      }
    }
}

void
f3 (void)
{
  #pragma omp for collapse (2)
  for (i = 0; i < 5; i++)
    {
      int k = foo ();
      {
	{
	  for (j = 0; j < 5; j++)
	    {
	    }
	}
      }
    }
}

void
f4 (void)
{
  #pragma omp for collapse (2)
  for (i = 0; i < 5; i++)
    {
      {
	for (j = 0; j < 5; j++)
	  ;
	foo ();
      }
    }
}

void
f5 (void)
{
  #pragma omp for collapse (2)
  for (i = 0; i < 5; i++)
    {
      {
	for (j = 0; j < 5; j++)
	  ;
      }
      foo ();
    }
}

void
f6 (void)
{
  #pragma omp for collapse (2)
  for (i = 0; i < 5; i++)
    {
      {
	for (j = 0; j < 5; j++)
	  ;
      }
    }
  foo ();
}
