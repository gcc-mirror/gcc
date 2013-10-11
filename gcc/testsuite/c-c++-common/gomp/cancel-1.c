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
  int i;
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
      #pragma omp cancel taskgroup
      #pragma omp cancellation point parallel	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point for	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point sections	/* { dg-error "not closely nested inside" } */
      #pragma omp cancellation point taskgroup
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
    #pragma omp target data
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
  #pragma omp target data
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
    #pragma omp cancel parallel			/* { dg-error "only distribute or parallel constructs are allowed to be closely nested" } */
    #pragma omp cancel for			/* { dg-error "only distribute or parallel constructs are allowed to be closely nested" } */
    #pragma omp cancel sections			/* { dg-error "only distribute or parallel constructs are allowed to be closely nested" } */
    #pragma omp cancel taskgroup		/* { dg-error "only distribute or parallel constructs are allowed to be closely nested" } */
    #pragma omp cancellation point parallel	/* { dg-error "only distribute or parallel constructs are allowed to be closely nested" } */
    #pragma omp cancellation point for		/* { dg-error "only distribute or parallel constructs are allowed to be closely nested" } */
    #pragma omp cancellation point sections	/* { dg-error "only distribute or parallel constructs are allowed to be closely nested" } */
    #pragma omp cancellation point taskgroup	/* { dg-error "only distribute or parallel constructs are allowed to be closely nested" } */
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
    #pragma omp target data
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
      #pragma omp target data
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
    #pragma omp target data
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
    #pragma omp target data
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
