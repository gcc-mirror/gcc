// { dg-do compile }

extern int test(int);
void foo()
{
  int i;

  for (i = 0; i < 10; ++i)
    {
      #pragma omp sections
      {
	continue;		// { dg-error "invalid exit" }
      }
    }

  #pragma omp sections
    {
    #pragma omp section
      { bad1: ; }		// { dg-error "jump to label" }
    #pragma omp section
      goto bad1;		// { dg-error "from here|enters OpenMP" }
    }

  #pragma omp sections
    {
      goto bad2;		// { dg-error "from here" }
    }
  bad2:;			// { dg-error "jump|exits OpenMP" }

  goto bad3;			// { dg-error "from here" }
  #pragma omp sections
    {
      bad3: ;			// { dg-error "jump|enters OpenMP" }
    }

  #pragma omp sections
    {
      goto ok1;
      ok1:;

    #pragma omp section
      for (i = 0; i < 10; ++i)
	if (test(i))
	  break;
	else
	  continue;

    #pragma omp section
      switch (i)
	{
	case 0:
	  break;
	default:
	  test(i);
	}
    }
}

// { dg-message "error: invalid branch to/from an OpenMP structured block" "" { target *-*-* } 21 }
// { dg-message "error: invalid branch to/from an OpenMP structured block" "" { target *-*-* } 26 }
// { dg-message "error: invalid entry to OpenMP structured block" "" { target *-*-* } 30 }
