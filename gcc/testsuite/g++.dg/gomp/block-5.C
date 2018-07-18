// { dg-do compile }

void foo()
{
  #pragma omp master
    {
      goto bad1;	// { dg-message "from here" }
    }

  #pragma omp master
    {
    bad1:		// { dg-error "jump" }
                        // { dg-message "exits OpenMP" "" { target *-*-* } .-1 }
      return;		// { dg-error "invalid exit" }
    }
}

// { dg-message "error: invalid branch to/from OpenMP structured block" "" { target *-*-* } 7 }
