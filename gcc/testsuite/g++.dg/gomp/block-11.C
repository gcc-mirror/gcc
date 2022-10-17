// { dg-do compile }

void foo()
{
  #pragma omp masked
    {
      goto bad1;	// { dg-message "from here" }
    }

  #pragma omp masked filter(1)
    {
    bad1:		// { dg-error "jump" }
			// { dg-message "exits OpenMP" "" { target *-*-* } .-1 }
      return;		// { dg-error "invalid exit" }
    }
}

// { dg-message "error: invalid branch to/from OpenMP structured block" "" { target *-*-* } 7 }
/* PR c++/24516 */
/* { dg-do compile } */

void
bar (int *p)
{
  int m;
#pragma omp parallel for
  for (m = 0; m < 1000; ++m)
    switch (p[m])
      {
      case 1:
	p[m] = 2;
	break;
      default:
	p[m] = 3;
	break;
      }
}
