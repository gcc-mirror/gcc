// { dg-do compile }

void foo()
{
  bad1:				// { dg-error "jump to label" }
  #pragma omp parallel
    goto bad1;			// { dg-message "from here|exits OpenMP" }

  goto bad2;			// { dg-message "from here" }
  #pragma omp parallel
    {
      bad2: ;			// { dg-error "jump to label" }
                                // { dg-message "enters OpenMP" "" { target *-*-* } .-1 }
    }

  #pragma omp parallel
    {
      int i;
      goto ok1;
      for (i = 0; i < 10; ++i)
	{ ok1: break; }
    }
}

// { dg-message "error: invalid branch to/from OpenMP structured block" "" { target *-*-* } 7 }
// { dg-message "error: invalid entry to OpenMP structured block" "" { target *-*-* } 9 }
