/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

void
f1 (void)
{
  int i, j;
  #pragma omp for
  for (i = 0; i < 3; i++)
    {
      #pragma omp for		/* { dg-error "may not be closely nested" } */
      for (j = 0; j < 3; j++)
	;
      #pragma omp sections	/* { dg-error "may not be closely nested" } */
      {
	;
      #pragma omp section
	;
      }
      #pragma omp single	/* { dg-error "may not be closely nested" } */
	;
    #pragma omp master		/* { dg-error "may not be closely nested" } */
      ;
      #pragma omp barrier	/* { dg-error "may not be closely nested" } */
    }
  #pragma omp sections
  {
    #pragma omp for		/* { dg-error "may not be closely nested" } */
    for (j = 0; j < 3; j++)
      ;
  }
  #pragma omp sections
  {
    #pragma omp sections	/* { dg-error "may not be closely nested" } */
    {
      ;
    #pragma omp section
      ;
    }
  }
  #pragma omp sections
  {
    #pragma omp single		/* { dg-error "may not be closely nested" } */
      ;
  }
  #pragma omp sections
  {
    #pragma omp master		/* { dg-error "may not be closely nested" } */
      ;
  }
  #pragma omp sections
  {
    #pragma omp section
      ;
  }
  #pragma omp sections
  {
    #pragma omp section
    #pragma omp for		/* { dg-error "may not be closely nested" } */
    for (j = 0; j < 3; j++)
      ;
  }
  #pragma omp sections
  {
    #pragma omp section
    #pragma omp sections	/* { dg-error "may not be closely nested" } */
    {
      ;
    #pragma omp section
      ;
    }
  }
  #pragma omp sections
  {
    #pragma omp section
    #pragma omp single		/* { dg-error "may not be closely nested" } */
      ;
  }
  #pragma omp sections
  {
    #pragma omp section
    #pragma omp master		/* { dg-error "may not be closely nested" } */
      ;
  }
  #pragma omp single
  {
    #pragma omp for		/* { dg-error "may not be closely nested" } */
    for (j = 0; j < 3; j++)
      ;
    #pragma omp sections	/* { dg-error "may not be closely nested" } */
    {
      ;
    #pragma omp section
      ;
    }
    #pragma omp single		/* { dg-error "may not be closely nested" } */
      ;
    #pragma omp master		/* { dg-error "may not be closely nested" } */
      ;
    #pragma omp barrier		/* { dg-error "may not be closely nested" } */
  }
  #pragma omp master
  {
    #pragma omp for		/* { dg-error "may not be closely nested" } */
    for (j = 0; j < 3; j++)
      ;
    #pragma omp sections	/* { dg-error "may not be closely nested" } */
    {
      ;
    #pragma omp section
      ;
    }
    #pragma omp single		/* { dg-error "may not be closely nested" } */
      ;
    #pragma omp master
      ;
    #pragma omp barrier		/* { dg-error "may not be closely nested" } */
  }
  #pragma omp task
  {
    #pragma omp for		/* { dg-error "may not be closely nested" } */
    for (j = 0; j < 3; j++)
      ;
    #pragma omp sections	/* { dg-error "may not be closely nested" } */
    {
      ;
    #pragma omp section
      ;
    }
    #pragma omp single		/* { dg-error "may not be closely nested" } */
      ;
    #pragma omp master		/* { dg-error "may not be closely nested" } */
      ;
    #pragma omp barrier		/* { dg-error "may not be closely nested" } */
  }
  #pragma omp parallel
  {
    #pragma omp for
    for (j = 0; j < 3; j++)
      ;
    #pragma omp sections
    {
      ;
    #pragma omp section
      ;
    }
    #pragma omp single
      ;
    #pragma omp master
      ;
    #pragma omp barrier
  }
}

void
f2 (void)
{
  int i, j;
  #pragma omp ordered
  {
    #pragma omp for		/* { dg-error "may not be closely nested" } */
    for (j = 0; j < 3; j++)
      ;
    #pragma omp sections	/* { dg-error "may not be closely nested" } */
    {
      ;
    #pragma omp section
      ;
    }
    #pragma omp single		/* { dg-error "may not be closely nested" } */
      ;
    #pragma omp master
      ;
    #pragma omp barrier		/* { dg-error "may not be closely nested" } */
  }
}

void
f3 (void)
{
  #pragma omp critical
  {
    #pragma omp ordered		/* { dg-error "may not be closely nested" } */
      ;
  }
}

void
f4 (void)
{
  #pragma omp task
  {
    #pragma omp ordered		/* { dg-error "may not be closely nested" } */
      ;
  }
}

void
f5 (void)
{
  int i;
  #pragma omp for
  for (i = 0; i < 10; i++)
    {
      #pragma omp ordered		/* { dg-error "must be closely nested" } */
	;
    }
  #pragma omp for ordered
  for (i = 0; i < 10; i++)
    {
      #pragma omp ordered
	;
    }
}

void
f6 (void)
{
  #pragma omp critical (foo)
    #pragma omp critical (bar)
      ;
  #pragma omp critical
    #pragma omp critical (baz)
      ;
}

void
f7 (void)
{
  #pragma omp critical (foo2)
    #pragma omp critical
      ;
  #pragma omp critical (bar)
    #pragma omp critical (bar)		/* { dg-error "may not be nested" } */
      ;
  #pragma omp critical
    #pragma omp critical		/* { dg-error "may not be nested" } */
      ;
}
