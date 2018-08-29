/* PR c/63326 */
/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

void
f1 (int x)
{
  int i;
  if (x)
    #pragma omp barrier				/* { dg-error "may only be used in compound statements" } */
  ;
  if (x)
    #pragma omp flush				/* { dg-error "may only be used in compound statements" } */
  ;
  if (x)
    #pragma omp taskwait			/* { dg-error "may only be used in compound statements" } */
  ;
  if (x)
    #pragma omp taskyield			/* { dg-error "may only be used in compound statements" } */
  ;
  #pragma omp parallel
  {
    if (x)
      #pragma omp cancel parallel		/* { dg-error "may only be used in compound statements" } */
    ;
  }
  #pragma omp parallel
  {
    if (x)
      #pragma omp cancellation point parallel	/* { dg-error "may only be used in compound statements" } */
    ;
  }
  #pragma omp for ordered(1)
  for (i = 0; i < 16; i++)
    {
      if (x)
	#pragma omp ordered depend(source)	/* { dg-error "may only be used in compound statements" } */
      ;
      if (x)
	#pragma omp ordered depend(sink: i-1)	/* { dg-error "may only be used in compound statements" } */
      ;
    }
  if (x)
    #pragma omp target enter data map(to:i)	/* { dg-error "may only be used in compound statements" } */
  ;
  if (x)
    #pragma omp target update to(i)		/* { dg-error "may only be used in compound statements" } */
  ;
  if (x)
    #pragma omp target exit data map(from:i)	/* { dg-error "may only be used in compound statements" } */
  ;
}

void
f2 (int x)
{
  int i;
  while (x)
    #pragma omp barrier				/* { dg-error "may only be used in compound statements" } */
  ;
  while (x)
    #pragma omp flush				/* { dg-error "may only be used in compound statements" } */
  ;
  while (x)
    #pragma omp taskwait			/* { dg-error "may only be used in compound statements" } */
  ;
  while (x)
    #pragma omp taskyield			/* { dg-error "may only be used in compound statements" } */
  ;
  #pragma omp parallel
  {
    while (x)
      #pragma omp cancel parallel		/* { dg-error "may only be used in compound statements" } */
    ;
  }
  #pragma omp parallel
  {
    while (x)
      #pragma omp cancellation point parallel	/* { dg-error "may only be used in compound statements" } */
    ;
  }
  #pragma omp for ordered(1)
  for (i = 0; i < 16; i++)
    {
      while (x)
	#pragma omp ordered depend(source)	/* { dg-error "may only be used in compound statements" } */
      ;
      while (x)
	#pragma omp ordered depend(sink: i-1)	/* { dg-error "may only be used in compound statements" } */
      ;
    }
  while (x)
    #pragma omp target enter data map(to:i)	/* { dg-error "may only be used in compound statements" } */
  ;
  while (x)
    #pragma omp target update to(i)		/* { dg-error "may only be used in compound statements" } */
  ;
  while (x)
    #pragma omp target exit data map(from:i)	/* { dg-error "may only be used in compound statements" } */
  ;
}

void
f3 (int x)
{
  int i;
  for (x = 0; x < 10; x++)
    #pragma omp barrier				/* { dg-error "may only be used in compound statements" } */
  ;
  for (x = 0; x < 10; x++)
    #pragma omp flush				/* { dg-error "may only be used in compound statements" } */
  ;
  for (x = 0; x < 10; x++)
    #pragma omp taskwait			/* { dg-error "may only be used in compound statements" } */
  ;
  for (x = 0; x < 10; x++)
    #pragma omp taskyield			/* { dg-error "may only be used in compound statements" } */
  ;
  #pragma omp parallel
  {
    for (x = 0; x < 10; x++)
      #pragma omp cancel parallel		/* { dg-error "may only be used in compound statements" } */
    ;
  }
  #pragma omp parallel
  {
    for (x = 0; x < 10; x++)
      #pragma omp cancellation point parallel	/* { dg-error "may only be used in compound statements" } */
    ;
  }
  #pragma omp for ordered(1)
  for (i = 0; i < 16; i++)
    {
      for (x = 0; x < 10; x++)
	#pragma omp ordered depend(source)	/* { dg-error "may only be used in compound statements" } */
      ;
      for (x = 0; x < 10; x++)
	#pragma omp ordered depend(sink: i-1)	/* { dg-error "may only be used in compound statements" } */
      ;
    }
  for (x = 0; x < 10; x++)
    #pragma omp target enter data map(to:i)	/* { dg-error "may only be used in compound statements" } */
  ;
  for (x = 0; x < 10; x++)
    #pragma omp target update to(i)		/* { dg-error "may only be used in compound statements" } */
  ;
  for (x = 0; x < 10; x++)
    #pragma omp target exit data map(from:i)	/* { dg-error "may only be used in compound statements" } */
  ;
}

void
f4 (int x)
{
  int i;
  {
    do
      #pragma omp barrier			/* { dg-error "may only be used in compound statements" } */
    while (0); /* { dg-error "before" "" { target c++ } } */
  } /* { dg-error "before" "" { target c++ } } */
  {
    do
      #pragma omp flush				/* { dg-error "may only be used in compound statements" } */
    while (0); /* { dg-error "before" "" { target c++ } } */
  } /* { dg-error "before" "" { target c++ } } */
  {
    do
      #pragma omp taskwait			/* { dg-error "may only be used in compound statements" } */
    while (0); /* { dg-error "before" "" { target c++ } } */
  } /* { dg-error "before" "" { target c++ } } */
  {
    do
      #pragma omp taskyield			/* { dg-error "may only be used in compound statements" } */
    while (0); /* { dg-error "before" "" { target c++ } } */
  } /* { dg-error "before" "" { target c++ } } */
  #pragma omp parallel
  {
    do
      #pragma omp cancel parallel		/* { dg-error "may only be used in compound statements" } */
    while (0); /* { dg-error "before" "" { target c++ } } */
  } /* { dg-error "before" "" { target c++ } } */
  #pragma omp parallel
  {
    do
      #pragma omp cancellation point parallel	/* { dg-error "may only be used in compound statements" } */
    while (0); /* { dg-error "before" "" { target c++ } } */
  } /* { dg-error "before" "" { target c++ } } */
  #pragma omp for ordered(1)
  for (i = 0; i < 16; i++)
    {
      {
	do
	  #pragma omp ordered depend(source)	/* { dg-error "may only be used in compound statements" } */
	while (0); /* { dg-error "before" "" { target c++ } } */
      } /* { dg-error "before" "" { target c++ } } */
      {
	do
	  #pragma omp ordered depend(sink: i-1)	/* { dg-error "may only be used in compound statements" } */
	while (0); /* { dg-error "before" "" { target c++ } } */
      } /* { dg-error "before" "" { target c++ } } */
    }
  {
    do
      #pragma omp target enter data map(to:i)	/* { dg-error "may only be used in compound statements" } */
    while (0); /* { dg-error "before" "" { target c++ } } */
  } /* { dg-error "before" "" { target c++ } } */
  {
    do
      #pragma omp target update to(i)		/* { dg-error "may only be used in compound statements" } */
    while (0); /* { dg-error "before" "" { target c++ } } */
  } /* { dg-error "before" "" { target c++ } } */
  {
    do
      #pragma omp target exit data map(from:i)	/* { dg-error "may only be used in compound statements" } */
    while (0); /* { dg-error "before" "" { target c++ } } */
  } /* { dg-error "before" "" { target c++ } } */
}

void
f5 (int x)
{
  int i;
  switch (x)
    #pragma omp barrier				/* { dg-error "may only be used in compound statements" } */
  ;
  switch (x)
    #pragma omp flush				/* { dg-error "may only be used in compound statements" } */
  ;
  switch (x)
    #pragma omp taskwait			/* { dg-error "may only be used in compound statements" } */
  ;
  switch (x)
    #pragma omp taskyield			/* { dg-error "may only be used in compound statements" } */
  ;
  #pragma omp parallel
  {
    switch (x)
      #pragma omp cancel parallel		/* { dg-error "may only be used in compound statements" } */
    ;
  }
  #pragma omp parallel
  {
    switch (x)
      #pragma omp cancellation point parallel	/* { dg-error "may only be used in compound statements" } */
    ;
  }
  #pragma omp for ordered(1)
  for (i = 0; i < 16; i++)
    {
      switch (x)
	#pragma omp ordered depend(source)	/* { dg-error "may only be used in compound statements" } */
      ;
      switch (x)
	#pragma omp ordered depend(sink: i-1)	/* { dg-error "may only be used in compound statements" } */
      ;
    }
  switch (x)
    #pragma omp target enter data map(to:i)	/* { dg-error "may only be used in compound statements" } */
  ;
  switch (x)
    #pragma omp target update to(i)		/* { dg-error "may only be used in compound statements" } */
  ;
  switch (x)
    #pragma omp target exit data map(from:i)	/* { dg-error "may only be used in compound statements" } */
  ;
}

void
f6 (int x)
{
  int i;
  switch (x)
    {
    case 1:
      #pragma omp barrier			/* { dg-error "may only be used in compound statements" } */
      ;
    }
  switch (x)
    {
    case 1:
      #pragma omp flush				/* { dg-error "may only be used in compound statements" } */
      ;
    }
  switch (x)
    {
    case 1:
      #pragma omp taskwait			/* { dg-error "may only be used in compound statements" } */
      ;
    }
  switch (x)
    {
    case 1:
      #pragma omp taskyield			/* { dg-error "may only be used in compound statements" } */
      ;
    }
  #pragma omp parallel
  {
    switch (x)
      {
      case 1:
	#pragma omp cancel parallel		/* { dg-error "may only be used in compound statements" } */
	;
      }
  }
  #pragma omp parallel
  {
    switch (x)
      {
      case 1:
	#pragma omp cancellation point parallel	/* { dg-error "may only be used in compound statements" } */
	;
      }
  }
  #pragma omp for ordered(1)
  for (i = 0; i < 16; i++)
    {
      switch (x)
	{
	case 1:
	  #pragma omp ordered depend(source)	/* { dg-error "may only be used in compound statements" } */
	  ;
	}
      switch (x)
	{
	case 1:
	  #pragma omp ordered depend(sink: i-1)	/* { dg-error "may only be used in compound statements" } */
	  ;
	}
    }
  switch (x)
    {
    case 1:
      #pragma omp target enter data map(to:i)	/* { dg-error "may only be used in compound statements" } */
      ;
    }
  switch (x)
    {
    case 1:
      #pragma omp target update to(i)		/* { dg-error "may only be used in compound statements" } */
      ;
    }
  switch (x)
    {
    case 1:
      #pragma omp target exit data map(from:i)	/* { dg-error "may only be used in compound statements" } */
      ;
    }
}

void
f7 (int x)
{
  int i;
  switch (x)
    {
    default:
      #pragma omp barrier			/* { dg-error "may only be used in compound statements" } */
      ;
    }
  switch (x)
    {
    default:
      #pragma omp flush				/* { dg-error "may only be used in compound statements" } */
      ;
    }
  switch (x)
    {
    default:
      #pragma omp taskwait			/* { dg-error "may only be used in compound statements" } */
      ;
    }
  switch (x)
    {
    default:
      #pragma omp taskyield			/* { dg-error "may only be used in compound statements" } */
      ;
    }
  #pragma omp parallel
  {
    switch (x)
      {
      default:
	#pragma omp cancel parallel		/* { dg-error "may only be used in compound statements" } */
	;
      }
  }
  #pragma omp parallel
  {
    switch (x)
      {
      default:
	#pragma omp cancellation point parallel	/* { dg-error "may only be used in compound statements" } */
	;
      }
  }
  #pragma omp for ordered(1)
  for (i = 0; i < 16; i++)
    {
      switch (x)
	{
	default:
	  #pragma omp ordered depend(source)	/* { dg-error "may only be used in compound statements" } */
	  ;
	}
      switch (x)
	{
	default:
	  #pragma omp ordered depend(sink: i-1)	/* { dg-error "may only be used in compound statements" } */
	  ;
	}
    }
  switch (x)
    {
    default:
      #pragma omp target enter data map(to:i)	/* { dg-error "may only be used in compound statements" } */
      ;
    }
  switch (x)
    {
    default:
      #pragma omp target update to(i)		/* { dg-error "may only be used in compound statements" } */
      ;
    }
  switch (x)
    {
    default:
      #pragma omp target exit data map(from:i)	/* { dg-error "may only be used in compound statements" } */
      ;
    }
}

void
f8 (int x)
{
  int i;
  lab1:
    #pragma omp barrier				/* { dg-error "may only be used in compound statements" } */
  ;
  lab2:
    #pragma omp flush				/* { dg-error "may only be used in compound statements" } */
  ;
  lab3:
    #pragma omp taskwait			/* { dg-error "may only be used in compound statements" } */
  ;
  lab4:
    #pragma omp taskyield			/* { dg-error "may only be used in compound statements" } */
  ;
  #pragma omp parallel
  {
    lab5:
      #pragma omp cancel parallel		/* { dg-error "may only be used in compound statements" } */
    ;
  }
  #pragma omp parallel
  {
    lab6:
      #pragma omp cancellation point parallel	/* { dg-error "may only be used in compound statements" } */
    ;
  }
  #pragma omp for ordered(1)
  for (i = 0; i < 16; i++)
    {
      lab7:
	#pragma omp ordered depend(source)	/* { dg-error "may only be used in compound statements" } */
      ;
      lab8:
	#pragma omp ordered depend(sink: i-1)	/* { dg-error "may only be used in compound statements" } */
      ;
    }
  lab9:
    #pragma omp target enter data map(to:i)	/* { dg-error "may only be used in compound statements" } */
  ;
  lab10:
    #pragma omp target update to(i)		/* { dg-error "may only be used in compound statements" } */
  ;
  lab11:
    #pragma omp target exit data map(from:i)	/* { dg-error "may only be used in compound statements" } */
  ;
}
