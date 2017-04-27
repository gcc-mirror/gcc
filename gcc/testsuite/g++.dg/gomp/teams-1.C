// { dg-do compile }

void
foo (int x)
{
  bad1:				// { dg-error "jump to label" }
  #pragma omp target teams
    goto bad1;			// { dg-message "from here|exits OpenMP" }

  goto bad2;			// { dg-message "from here" }
  #pragma omp target teams
    {
      bad2: ;			// { dg-error "jump to label" }
                                // { dg-message "enters OpenMP" "" { target *-*-* } .-1 }
    }

  #pragma omp target teams
    {
      int i;
      goto ok1;
      for (i = 0; i < 10; ++i)
	{ ok1: break; }
    }

  switch (x)
  {
  #pragma omp target teams
    { case 0:; }		// { dg-error "jump" }
				// { dg-warning "statement will never be executed" "" { target *-*-* } 27 }
                                // { dg-message "enters" "" { target *-*-* } 28 }
  }
}

void
bar (int x)
{
  bad1:				// { dg-error "jump to label" }
  #pragma omp target
  #pragma omp teams
    goto bad1;			// { dg-message "from here|exits OpenMP" }

  goto bad2;			// { dg-message "from here" }
  #pragma omp target
  #pragma omp teams
    {
      bad2: ;			// { dg-error "jump to label" }
                                // { dg-message "enters OpenMP" "" { target *-*-* } .-1 }
    }

  #pragma omp target
  #pragma omp teams
    {
      int i;
      goto ok1;
      for (i = 0; i < 10; ++i)
	{ ok1: break; }
    }

  switch (x)
  {
  #pragma omp target		// { dg-warning "statement will never be executed" }
  #pragma omp teams
    { case 0:; }		// { dg-error "jump" }
                                // { dg-message "enters" "" { target *-*-* } .-1 }
  }
}

// { dg-error "invalid branch to/from OpenMP structured block" "" { target *-*-* } 8 }
// { dg-error "invalid entry to OpenMP structured block" "" { target *-*-* } 10 }
// { dg-error "invalid branch to/from OpenMP structured block" "" { target *-*-* } 40 }
// { dg-error "invalid entry to OpenMP structured block" "" { target *-*-* } 42 }
