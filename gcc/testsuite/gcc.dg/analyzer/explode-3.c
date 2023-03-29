/* { dg-additional-options "-Wno-analyzer-too-complex" } */

#include <stdlib.h>

extern int get (void);

/* In theory each of p0...p1 can be in various malloc states,
   independently, so the total combined number of states
   at any program point within the loop is NUM_VARS * NUM_STATES.  */

void test (void)
{
  void *p0, *p1;
  void **pp;
  while (get ())
    {
      switch (get ())
	{
	default:
	case 0:
	  pp = &p0;
	  break;
	case 1:
	  pp = &p1;
	  break;
	}

      switch (get ())
	{
	default:
	case 0:
	  *pp = malloc (16); /* { dg-warning "leak" "" { xfail *-*-* } } */
	  // TODO: xfail
	  break;
	case 1:
	  free (*pp);
	  break;
	case 2:
	  /* no-op.  */
	  break;
	}
    }
}
