// { dg-do compile }

void
foo (int x)
{
  bad1:				// { dg-error "jump to label" }
  #pragma omp taskgroup
    goto bad1;			// { dg-message "from here|exits OpenMP" }

  goto bad2;			// { dg-message "from here" }
  #pragma omp taskgroup
    {
      bad2: ;			// { dg-error "jump to label" }
                                // { dg-message "enters OpenMP" "" { target *-*-* } 13 }
    }

  #pragma omp taskgroup
    {
      int i;
      goto ok1;
      for (i = 0; i < 10; ++i)
	{ ok1: break; }
    }

  switch (x)
  {
  #pragma omp taskgroup		// { dg-warning "statement will never be executed" }
    { case 0:; }		// { dg-error "jump" }
                                // { dg-message "enters" "" { target *-*-* } 28 }
  }
}

// { dg-error "invalid branch to/from OpenMP structured block" "" { target *-*-* } 8 }
// { dg-error "invalid entry to OpenMP structured block" "" { target *-*-* } 10 }
