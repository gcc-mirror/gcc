// { dg-do compile }

void foo()
{
  bad1:				// { dg-error "jump to label" }
  #pragma omp parallel
    goto bad1;			// { dg-error "from here|exits OpenMP" }

  goto bad2;			// { dg-error "from here" }
  #pragma omp parallel
    {
      bad2: ;			// { dg-error "jump to label|enters OpenMP" }
    }

  #pragma omp parallel
    {
      int i;
      goto ok1;
      for (i = 0; i < 10; ++i)
	{ ok1: break; }
    }
}

// { dg-message "error: invalid branch to/from an OpenMP structured block" "" { target *-*-* } 7 }
// { dg-message "error: invalid entry to OpenMP structured block" "" { target *-*-* } 9 }
