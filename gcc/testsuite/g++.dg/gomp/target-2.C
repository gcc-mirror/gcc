// { dg-do compile }

void
foo (int x, int y)
{
  bad1:				// { dg-error "jump to label" }
  #pragma omp target data map(tofrom: y)
    goto bad1;			// { dg-message "from here|exits OpenMP" }

  goto bad2;			// { dg-message "from here" }
  #pragma omp target data map(tofrom: y)
    {
      bad2: ;			// { dg-error "jump to label" }
                                // { dg-message "enters OpenMP" "" { target *-*-* } .-1 }
    }

  #pragma omp target data map(tofrom: y)
    {
      int i;
      goto ok1;
      for (i = 0; i < 10; ++i)
	{ ok1: break; }
    }

  switch (x)
  {
  #pragma omp target data map(tofrom: y) // { dg-warning "statement will never be executed" }
    { case 0:; }		// { dg-error "jump" }
                                // { dg-message "enters" "" { target *-*-* } .-1 }
  }
}

// { dg-error "invalid branch to/from OpenMP structured block" "" { target *-*-* } 8 }
// { dg-error "invalid entry to OpenMP structured block" "" { target *-*-* } 10 }
