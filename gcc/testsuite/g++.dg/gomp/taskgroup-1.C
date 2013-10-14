// { dg-do compile }

void
foo (int x)
{
  bad1:				// { dg-error "jump to label" }
  #pragma omp taskgroup
    goto bad1;			// { dg-error "from here|exits OpenMP" }

  goto bad2;			// { dg-error "from here" }
  #pragma omp taskgroup
    {
      bad2: ;			// { dg-error "jump to label|enters OpenMP" }
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
  #pragma omp taskgroup
    { case 0:; }		// { dg-error "jump|enters" }
  }
}

// { dg-error "invalid branch to/from an OpenMP structured block" "" { target *-*-* } 8 }
// { dg-error "invalid entry to OpenMP structured block" "" { target *-*-* } 10 }
