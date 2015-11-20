void
foo (void)
{
  int i;
  #pragma omp taskloop
  for (i = 0; i < 64; i++)
    {
      int j;
      #pragma omp for			/* { dg-error "region may not be closely nested inside of" } */
      for (j = 0; j < 10; j++)
	;
      #pragma omp single		/* { dg-error "region may not be closely nested inside of" } */
      ;
      #pragma omp sections		/* { dg-error "region may not be closely nested inside of" } */
      {
	#pragma omp section
	;
      }
      #pragma omp barrier		/* { dg-error "region may not be closely nested inside of" } */
      #pragma omp master		/* { dg-error "region may not be closely nested inside of" } */
      ;
      #pragma omp ordered		/* { dg-error "region may not be closely nested inside of" } */
      ;
      #pragma omp ordered threads	/* { dg-error "region may not be closely nested inside of" } */
      ;
      #pragma omp ordered simd threads	/* { dg-error ".ordered. .simd. must be closely nested inside .simd. region" } */
      ;
      #pragma omp simd
      for (j = 0; j < 10; j++)
	#pragma omp ordered simd
	  ;
      #pragma omp critical
      {
	#pragma omp simd
	for (j = 0; j < 10; j++)
	  #pragma omp ordered simd
	    ;
      }
    }
  #pragma omp taskloop
  for (i = 0; i < 64; i++)
    #pragma omp parallel
    {
      int j;
      #pragma omp for
      for (j = 0; j < 10; j++)
	;
      #pragma omp single
      ;
      #pragma omp sections
      {
	#pragma omp section
	;
      }
      #pragma omp barrier
      #pragma omp master
      ;
      #pragma omp ordered		/* { dg-error ".ordered. region must be closely nested inside a loop region with an .ordered. clause" } */
      ;
      #pragma omp ordered threads	/* { dg-error ".ordered. region must be closely nested inside a loop region with an .ordered. clause" } */
      ;
      #pragma omp simd
      for (j = 0; j < 10; j++)
	#pragma omp ordered simd
	  ;
      #pragma omp critical
      {
	#pragma omp simd
	for (j = 0; j < 10; j++)
	  #pragma omp ordered simd
	    ;
      }
    }
  #pragma omp taskloop
  for (i = 0; i < 64; i++)
    #pragma omp target
    {
      int j;
      #pragma omp for
      for (j = 0; j < 10; j++)
	;
      #pragma omp single
      ;
      #pragma omp sections
      {
	#pragma omp section
	;
      }
      #pragma omp barrier
      #pragma omp master
      ;
      #pragma omp ordered		/* { dg-error ".ordered. region must be closely nested inside a loop region with an .ordered. clause" } */
      ;
      #pragma omp ordered threads	/* { dg-error ".ordered. region must be closely nested inside a loop region with an .ordered. clause" } */
      ;
      #pragma omp simd
      for (j = 0; j < 10; j++)
	#pragma omp ordered simd
	  ;
      #pragma omp critical
      {
	#pragma omp simd
	for (j = 0; j < 10; j++)
	  #pragma omp ordered simd
	    ;
      }
    }
  #pragma omp ordered
  {
    #pragma omp ordered			/* { dg-error "region may not be closely nested inside of" } */
    ;
  }
  #pragma omp ordered threads
  {
    #pragma omp ordered			/* { dg-error "region may not be closely nested inside of" } */
    ;
  }
  #pragma omp ordered
  {
    #pragma omp ordered threads		/* { dg-error "region may not be closely nested inside of" } */
    ;
  }
  #pragma omp ordered threads
  {
    #pragma omp ordered threads		/* { dg-error "region may not be closely nested inside of" } */
    ;
  }
  #pragma omp critical
  {
    #pragma omp ordered simd		/* { dg-error ".ordered. .simd. must be closely nested inside .simd. region" } */
    ;
  }
  #pragma omp for ordered
  for (i = 0; i < 64; i++)
    #pragma omp parallel
    {
      #pragma omp ordered threads	/* { dg-error ".ordered. region must be closely nested inside a loop region with an .ordered. clause" } */
      ;
    }
  #pragma omp for ordered
  for (i = 0; i < 64; i++)
    #pragma omp parallel
    {
      #pragma omp ordered		/* { dg-error ".ordered. region must be closely nested inside a loop region with an .ordered. clause" } */
      ;
    }
  #pragma omp for ordered(1)
  for (i = 0; i < 64; i++)
    #pragma omp parallel
    {
      #pragma omp ordered depend(source)	/* { dg-error ".ordered. construct with .depend. clause must be closely nested inside a loop with .ordered. clause with a parameter" } */
      #pragma omp ordered depend(sink: i - 1)	/* { dg-error ".ordered. construct with .depend. clause must be closely nested inside a loop with .ordered. clause with a parameter" } */
    }
}
