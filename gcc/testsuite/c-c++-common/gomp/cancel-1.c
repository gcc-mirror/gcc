/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

void
f1 (void)
{
  #pragma omp cancel parallel			/* { dg-error "orphaned" } */
  #pragma omp cancel for			/* { dg-error "orphaned" } */
  #pragma omp cancel sections			/* { dg-error "orphaned" } */
  #pragma omp cancel taskgroup			/* { dg-error "orphaned" } */
  #pragma omp cancellation point parallel	/* { dg-error "orphaned" } */
  #pragma omp cancellation point for		/* { dg-error "orphaned" } */
  #pragma omp cancellation point sections	/* { dg-error "orphaned" } */
  #pragma omp cancellation point taskgroup	/* { dg-error "orphaned" } */
}

void
f2 (void)
{
  int i, j = 0;
  #pragma omp parallel
  {
    #pragma omp cancel parallel
    #pragma omp cancel for			/* { dg-error "not closely nested inside" } */
    #pragma omp cancel sections			/* { dg-error "not closely nested inside" } */
    #pragma omp cancel taskgroup		/* { dg-error "not closely nested inside" } */
    #pragma omp cancellation point parallel
    #pragma omp cancellation point for		/* { dg-error "not closely nested inside" } */
    #pragma omp cancellation point sections	/* { dg-error "not closely nested inside" } */
    #pragma omp cancellation point taskgroup	/* { dg-error "not closely nested inside" } */
    #pragma omp master
    {
      #pragma omp cancel parallel		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel for			/* { dg-error "not closely nested inside" } */
      #pragma omp cancel sections		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel taskgroup		/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point parallel	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point for	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point sections	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point taskgroup	/* { dg-error "not closely nested inside" } */
    }
    #pragma omp masked
    {
      #pragma omp cancel parallel		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel for			/* { dg-error "not closely nested inside" } */
      #pragma omp cancel sections		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel taskgroup		/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point parallel	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point for	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point sections	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point taskgroup	/* { dg-error "not closely nested inside" } */
    }
    #pragma omp scope
    {
      #pragma omp cancel parallel		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel for			/* { dg-error "not closely nested inside" } */
      #pragma omp cancel sections		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel taskgroup		/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point parallel	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point for	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point sections	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point taskgroup	/* { dg-error "not closely nested inside" } */
    }
    #pragma omp single
    {
      #pragma omp cancel parallel		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel for			/* { dg-error "not closely nested inside" } */
      #pragma omp cancel sections		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel taskgroup		/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point parallel	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point for	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point sections	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point taskgroup	/* { dg-error "not closely nested inside" } */
    }
    #pragma omp critical
    {
      #pragma omp cancel parallel		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel for			/* { dg-error "not closely nested inside" } */
      #pragma omp cancel sections		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel taskgroup		/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point parallel	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point for	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point sections	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point taskgroup	/* { dg-error "not closely nested inside" } */
    }
    #pragma omp taskgroup
    {
      #pragma omp cancel parallel		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel for			/* { dg-error "not closely nested inside" } */
      #pragma omp cancel sections		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel taskgroup		/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point parallel	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point for	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point sections	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point taskgroup	/* { dg-error "not closely nested inside" } */
    }
    #pragma omp task
    {
      #pragma omp cancel parallel		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel for			/* { dg-error "not closely nested inside" } */
      #pragma omp cancel sections		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel taskgroup		/* { dg-error "construct not closely nested inside of .taskgroup. region" } */
      #pragma omp cancellation point parallel	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point for	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point sections	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point taskgroup	/* { dg-error "construct not closely nested inside of .taskgroup. region" } */
    }
    #pragma omp taskgroup
    #pragma omp task
    {
      #pragma omp cancel parallel		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel for			/* { dg-error "not closely nested inside" } */
      #pragma omp cancel sections		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel taskgroup
      #pragma omp cancellation point parallel	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point for	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point sections	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point taskgroup
    }
    #pragma omp taskloop
    for (i = 0; i < 10; i++)
      {
        #pragma omp cancel parallel		/* { dg-error "not closely nested inside" } */
        #pragma omp cancel for			/* { dg-error "not closely nested inside" } */
        #pragma omp cancel sections		/* { dg-error "not closely nested inside" } */
        #pragma omp cancel taskgroup
        #pragma omp cancellation point parallel	/* { dg-error "not closely nested inside" } */
        #pragma omp cancellation point for	/* { dg-error "not closely nested inside" } */
        #pragma omp cancellation point sections	/* { dg-error "not closely nested inside" } */
        #pragma omp cancellation point taskgroup
	#pragma omp task
	{
	  #pragma omp cancellation point taskgroup
	  #pragma omp cancel taskgroup
	}
      }
    #pragma omp taskloop nogroup
    for (i = 0; i < 10; i++)
      {
        #pragma omp cancel parallel		/* { dg-error "not closely nested inside" } */
        #pragma omp cancel for			/* { dg-error "not closely nested inside" } */
        #pragma omp cancel sections		/* { dg-error "not closely nested inside" } */
        #pragma omp cancel taskgroup		/* { dg-error "construct not closely nested inside of .taskgroup. region" } */
        #pragma omp cancellation point parallel	/* { dg-error "not closely nested inside" } */
        #pragma omp cancellation point for	/* { dg-error "not closely nested inside" } */
        #pragma omp cancellation point sections	/* { dg-error "not closely nested inside" } */
        #pragma omp cancellation point taskgroup/* { dg-error "construct not closely nested inside of .taskgroup. region" } */
	#pragma omp task
	{
	  #pragma omp cancellation point taskgroup/* { dg-error "construct not closely nested inside of .taskgroup. region" } */
	  #pragma omp cancel taskgroup		/* { dg-error "construct not closely nested inside of .taskgroup. region" } */
	}
      }
    #pragma omp taskgroup
    {
      #pragma omp task
      {
	#pragma omp task
	{
	  #pragma omp cancellation point taskgroup
	  #pragma omp cancel taskgroup
	}
      }
      #pragma omp taskloop nogroup
      for (i = 0; i < 10; i++)
	{
	  #pragma omp task
	  {
	    #pragma omp cancellation point taskgroup
	    #pragma omp cancel taskgroup
	  }
	  #pragma omp cancellation point taskgroup
	  #pragma omp cancel taskgroup
	}
    }
    #pragma omp taskgroup
    {
      #pragma omp parallel
      {
	#pragma omp task
	{
	  #pragma omp cancel taskgroup		/* { dg-error "construct not closely nested inside of .taskgroup. region" } */
	  #pragma omp cancellation point taskgroup /* { dg-error "construct not closely nested inside of .taskgroup. region" } */
	}
	#pragma omp taskloop
	for (i = 0; i < 10; i++)
	  {
	    #pragma omp cancel taskgroup
	    #pragma omp cancellation point taskgroup
	  }
	#pragma omp taskloop nogroup
	for (i = 0; i < 10; i++)
	  {
	    #pragma omp cancel taskgroup	     /* { dg-error "construct not closely nested inside of .taskgroup. region" } */
	    #pragma omp cancellation point taskgroup /* { dg-error "construct not closely nested inside of .taskgroup. region" } */
	  }
      }
      #pragma omp target
      {
	#pragma omp task
	{
	  #pragma omp cancel taskgroup		/* { dg-error "construct not closely nested inside of .taskgroup. region" } */
	  #pragma omp cancellation point taskgroup /* { dg-error "construct not closely nested inside of .taskgroup. region" } */
	}
      }
      #pragma omp target
      #pragma omp teams
      #pragma omp distribute
      for (i = 0; i < 10; i++)
	{
	  #pragma omp task
	  {
	    #pragma omp cancel taskgroup	/* { dg-error "construct not closely nested inside of .taskgroup. region" } */
	    #pragma omp cancellation point taskgroup /* { dg-error "construct not closely nested inside of .taskgroup. region" } */
	  }
	}
      #pragma omp target data map(i)
      {
	#pragma omp task
	{
	  #pragma omp cancel taskgroup
	  #pragma omp cancellation point taskgroup
	}
      }
    }
    #pragma omp taskloop
    for (i = 0; i < 10; i++)
      {
	#pragma omp parallel
	{
	  #pragma omp task
	  {
	    #pragma omp cancel taskgroup	     /* { dg-error "construct not closely nested inside of .taskgroup. region" } */
	    #pragma omp cancellation point taskgroup /* { dg-error "construct not closely nested inside of .taskgroup. region" } */
	  }
	}
        #pragma omp target
	{
	  #pragma omp task
	  {
	    #pragma omp cancel taskgroup	     /* { dg-error "construct not closely nested inside of .taskgroup. region" } */
	    #pragma omp cancellation point taskgroup /* { dg-error "construct not closely nested inside of .taskgroup. region" } */
	  }
	}
	#pragma omp target
	#pragma omp teams
	#pragma omp distribute
	for (j = 0; j < 10; j++)
	  {
	    #pragma omp task
	    {
	      #pragma omp cancel taskgroup	/* { dg-error "construct not closely nested inside of .taskgroup. region" } */
	      #pragma omp cancellation point taskgroup /* { dg-error "construct not closely nested inside of .taskgroup. region" } */
	    }
	  }
	#pragma omp target data map(i)
	{
	  #pragma omp task
	  {
	    #pragma omp cancel taskgroup
	    #pragma omp cancellation point taskgroup
	  }
	}
      }
    #pragma omp for
    for (i = 0; i < 10; i++)
      {
	#pragma omp cancel parallel		/* { dg-error "not closely nested inside" } */
	#pragma omp cancel for
	#pragma omp cancel sections		/* { dg-error "not closely nested inside" } */
	#pragma omp cancel taskgroup		/* { dg-error "not closely nested inside" } */
	#pragma omp cancellation point parallel	/* { dg-error "not closely nested inside" } */
	#pragma omp cancellation point for
	#pragma omp cancellation point sections	/* { dg-error "not closely nested inside" } */
	#pragma omp cancellation point taskgroup/* { dg-error "not closely nested inside" } */
      }
    #pragma omp for ordered
    for (i = 0; i < 10; i++)
      #pragma omp ordered
      {
	#pragma omp cancel parallel		/* { dg-error "not closely nested inside" } */
	#pragma omp cancel for			/* { dg-error "not closely nested inside" } */
	#pragma omp cancel sections		/* { dg-error "not closely nested inside" } */
	#pragma omp cancel taskgroup		/* { dg-error "not closely nested inside" } */
	#pragma omp cancellation point parallel	/* { dg-error "not closely nested inside" } */
	#pragma omp cancellation point for	/* { dg-error "not closely nested inside" } */
	#pragma omp cancellation point sections	/* { dg-error "not closely nested inside" } */
	#pragma omp cancellation point taskgroup/* { dg-error "not closely nested inside" } */
      }
    #pragma omp sections
    {
      {
	#pragma omp cancel parallel		/* { dg-error "not closely nested inside" } */
	#pragma omp cancel for			/* { dg-error "not closely nested inside" } */
	#pragma omp cancel sections
	#pragma omp cancel taskgroup		/* { dg-error "not closely nested inside" } */
	#pragma omp cancellation point parallel	/* { dg-error "not closely nested inside" } */
	#pragma omp cancellation point for	/* { dg-error "not closely nested inside" } */
	#pragma omp cancellation point sections
	#pragma omp cancellation point taskgroup/* { dg-error "not closely nested inside" } */
      }
      #pragma omp section
      {
	#pragma omp cancel parallel		/* { dg-error "not closely nested inside" } */
	#pragma omp cancel for			/* { dg-error "not closely nested inside" } */
	#pragma omp cancel sections
	#pragma omp cancel taskgroup		/* { dg-error "not closely nested inside" } */
	#pragma omp cancellation point parallel	/* { dg-error "not closely nested inside" } */
	#pragma omp cancellation point for	/* { dg-error "not closely nested inside" } */
	#pragma omp cancellation point sections
	#pragma omp cancellation point taskgroup/* { dg-error "not closely nested inside" } */
      }
    }
    #pragma omp target data map(j)
    {
      #pragma omp cancel parallel		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel for			/* { dg-error "not closely nested inside" } */
      #pragma omp cancel sections		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel taskgroup		/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point parallel	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point for	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point sections	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point taskgroup	/* { dg-error "not closely nested inside" } */
    }
    #pragma omp target
    {
      #pragma omp cancel parallel		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel for			/* { dg-error "not closely nested inside" } */
      #pragma omp cancel sections		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel taskgroup		/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point parallel	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point for	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point sections	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point taskgroup	/* { dg-error "not closely nested inside" } */
    }
  }
  #pragma omp target data map(j)
  {
    #pragma omp cancel parallel			/* { dg-error "not closely nested inside" } */
    #pragma omp cancel for			/* { dg-error "not closely nested inside" } */
    #pragma omp cancel sections			/* { dg-error "not closely nested inside" } */
    #pragma omp cancel taskgroup		/* { dg-error "not closely nested inside" } */
    #pragma omp cancellation point parallel	/* { dg-error "not closely nested inside" } */
    #pragma omp cancellation point for		/* { dg-error "not closely nested inside" } */
    #pragma omp cancellation point sections	/* { dg-error "not closely nested inside" } */
    #pragma omp cancellation point taskgroup	/* { dg-error "not closely nested inside" } */
  }
  #pragma omp target
  {
    #pragma omp cancel parallel			/* { dg-error "not closely nested inside" } */
    #pragma omp cancel for			/* { dg-error "not closely nested inside" } */
    #pragma omp cancel sections			/* { dg-error "not closely nested inside" } */
    #pragma omp cancel taskgroup		/* { dg-error "not closely nested inside" } */
    #pragma omp cancellation point parallel	/* { dg-error "not closely nested inside" } */
    #pragma omp cancellation point for		/* { dg-error "not closely nested inside" } */
    #pragma omp cancellation point sections	/* { dg-error "not closely nested inside" } */
    #pragma omp cancellation point taskgroup	/* { dg-error "not closely nested inside" } */
  }
  #pragma omp target teams
  {
    #pragma omp cancel parallel			/* { dg-error "only .distribute., .parallel. or .loop. regions are allowed to be strictly nested" } */
    #pragma omp cancel for			/* { dg-error "only .distribute., .parallel. or .loop. regions are allowed to be strictly nested" } */
    #pragma omp cancel sections			/* { dg-error "only .distribute., .parallel. or .loop. regions are allowed to be strictly nested" } */
    #pragma omp cancel taskgroup		/* { dg-error "only .distribute., .parallel. or .loop. regions are allowed to be strictly nested" } */
    #pragma omp cancellation point parallel	/* { dg-error "only .distribute., .parallel. or .loop. regions are allowed to be strictly nested" } */
    #pragma omp cancellation point for		/* { dg-error "only .distribute., .parallel. or .loop. regions are allowed to be strictly nested" } */
    #pragma omp cancellation point sections	/* { dg-error "only .distribute., .parallel. or .loop. regions are allowed to be strictly nested" } */
    #pragma omp cancellation point taskgroup	/* { dg-error "only .distribute., .parallel. or .loop. regions are allowed to be strictly nested" } */
  }
  #pragma omp target teams distribute
  for (i = 0; i < 10; i++)
    {
      #pragma omp cancel parallel		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel for			/* { dg-error "not closely nested inside" } */
      #pragma omp cancel sections		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel taskgroup		/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point parallel	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point for	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point sections	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point taskgroup	/* { dg-error "not closely nested inside" } */
    }
  #pragma omp for
  for (i = 0; i < 10; i++)
    {
      #pragma omp cancel parallel		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel for
      #pragma omp cancel sections		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel taskgroup		/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point parallel	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point for
      #pragma omp cancellation point sections	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point taskgroup	/* { dg-error "not closely nested inside" } */
    }
  #pragma omp for
  for (i = 0; i < 10; i++)
    #pragma omp target data map(j)
    {
      #pragma omp cancel parallel		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel for			/* { dg-error "not closely nested inside" } */
      #pragma omp cancel sections		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel taskgroup		/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point parallel	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point for	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point sections	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point taskgroup	/* { dg-error "not closely nested inside" } */
    }
  #pragma omp for
  for (i = 0; i < 10; i++)
    #pragma omp target
    {
      #pragma omp cancel parallel		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel for			/* { dg-error "not closely nested inside" } */
      #pragma omp cancel sections		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel taskgroup		/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point parallel	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point for	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point sections	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point taskgroup	/* { dg-error "not closely nested inside" } */
    }
  #pragma omp for ordered
  for (i = 0; i < 10; i++)
    #pragma omp ordered
      #pragma omp target data map(j)
      {
	#pragma omp cancel parallel		/* { dg-error "not closely nested inside" } */
	#pragma omp cancel for			/* { dg-error "not closely nested inside" } */
	#pragma omp cancel sections		/* { dg-error "not closely nested inside" } */
	#pragma omp cancel taskgroup		/* { dg-error "not closely nested inside" } */
	#pragma omp cancellation point parallel	/* { dg-error "not closely nested inside" } */
	#pragma omp cancellation point for	/* { dg-error "not closely nested inside" } */
	#pragma omp cancellation point sections	/* { dg-error "not closely nested inside" } */
	#pragma omp cancellation point taskgroup/* { dg-error "not closely nested inside" } */
      }
  #pragma omp for ordered
  for (i = 0; i < 10; i++)
    #pragma omp ordered
      #pragma omp target
      {
	#pragma omp cancel parallel		/* { dg-error "not closely nested inside" } */
	#pragma omp cancel for			/* { dg-error "not closely nested inside" } */
	#pragma omp cancel sections		/* { dg-error "not closely nested inside" } */
	#pragma omp cancel taskgroup		/* { dg-error "not closely nested inside" } */
	#pragma omp cancellation point parallel	/* { dg-error "not closely nested inside" } */
	#pragma omp cancellation point for	/* { dg-error "not closely nested inside" } */
	#pragma omp cancellation point sections	/* { dg-error "not closely nested inside" } */
	#pragma omp cancellation point taskgroup/* { dg-error "not closely nested inside" } */
      }
  #pragma omp sections
  {
    {
      #pragma omp cancel parallel		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel for			/* { dg-error "not closely nested inside" } */
      #pragma omp cancel sections
      #pragma omp cancel taskgroup		/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point parallel	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point for	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point sections
      #pragma omp cancellation point taskgroup	/* { dg-error "not closely nested inside" } */
    }
    #pragma omp section
    {
      #pragma omp cancel parallel		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel for			/* { dg-error "not closely nested inside" } */
      #pragma omp cancel sections
      #pragma omp cancel taskgroup		/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point parallel	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point for	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point sections
      #pragma omp cancellation point taskgroup	/* { dg-error "not closely nested inside" } */
    }
  }
  #pragma omp sections
  {
    #pragma omp target data map(j)
    {
      #pragma omp cancel parallel		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel for			/* { dg-error "not closely nested inside" } */
      #pragma omp cancel sections		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel taskgroup		/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point parallel	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point for	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point sections	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point taskgroup	/* { dg-error "not closely nested inside" } */
    }
    #pragma omp section
    #pragma omp target data map(j)
    {
      #pragma omp cancel parallel		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel for			/* { dg-error "not closely nested inside" } */
      #pragma omp cancel sections		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel taskgroup		/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point parallel	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point for	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point sections	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point taskgroup	/* { dg-error "not closely nested inside" } */
    }
  }
  #pragma omp sections
  {
    #pragma omp target
    {
      #pragma omp cancel parallel		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel for			/* { dg-error "not closely nested inside" } */
      #pragma omp cancel sections		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel taskgroup		/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point parallel	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point for	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point sections	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point taskgroup	/* { dg-error "not closely nested inside" } */
    }
    #pragma omp section
    #pragma omp target
    {
      #pragma omp cancel parallel		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel for			/* { dg-error "not closely nested inside" } */
      #pragma omp cancel sections		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel taskgroup		/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point parallel	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point for	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point sections	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point taskgroup	/* { dg-error "not closely nested inside" } */
    }
  }
  #pragma omp task
  {
    #pragma omp cancel parallel			/* { dg-error "not closely nested inside" } */
    #pragma omp cancel for			/* { dg-error "not closely nested inside" } */
    #pragma omp cancel sections			/* { dg-error "not closely nested inside" } */
    #pragma omp cancel taskgroup
    #pragma omp cancellation point parallel	/* { dg-error "not closely nested inside" } */
    #pragma omp cancellation point for		/* { dg-error "not closely nested inside" } */
    #pragma omp cancellation point sections	/* { dg-error "not closely nested inside" } */
    #pragma omp cancellation point taskgroup
    #pragma omp taskgroup
    {
      #pragma omp cancel parallel		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel for			/* { dg-error "not closely nested inside" } */
      #pragma omp cancel sections		/* { dg-error "not closely nested inside" } */
      #pragma omp cancel taskgroup		/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point parallel	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point for	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point sections	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point taskgroup	/* { dg-error "not closely nested inside" } */
    }
  }
}

void
f3 (void)
{
  int i;
  #pragma omp for nowait
  for (i = 0; i < 10; i++)
    {
      #pragma omp cancel for		/* { dg-warning "nowait" } */
    }
  #pragma omp sections nowait
  {
    {
      #pragma omp cancel sections	/* { dg-warning "nowait" } */
    }
    #pragma omp section
    {
      #pragma omp cancel sections	/* { dg-warning "nowait" } */
    }
  }
  #pragma omp for ordered
  for (i = 0; i < 10; i++)
    {
      #pragma omp cancel for		/* { dg-warning "ordered" } */
      #pragma omp ordered
      {
      }
    }
}

#pragma omp cancellation point /* { dg-error "expected declaration specifiers before end of line" } */

void
f4 (void)
{
  if (0)
#pragma omp cancellation EKAHI /* { dg-error "expected .point. before .EKAHI." } */
    ;
#pragma omp cancellation HO OKAHI /* { dg-error "expected .point. before .HO." } */
  if (0)
#pragma omp cancellation point /* { dg-error ".pragma omp cancellation point. may only be used in compound statements" } */
    ;
#pragma omp cancellation point /* { dg-error ".pragma omp cancellation point. must specify one of" } */
}
