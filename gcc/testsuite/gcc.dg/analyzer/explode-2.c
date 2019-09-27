/* In theory each of p0...p3 can be in various malloc states,
   independently, so the total combined number of states
   at any program point within the loop is NUM_VARS * NUM_STATES.

   Set the limits high enough that we can fully explore this.  */ 

/* { dg-additional-options "--param analyzer-max-enodes-per-program-point=200 --param analyzer-bb-explosion-factor=50" } */

#include <stdlib.h>

extern int get (void);

void test (void)
{
  void *p0, *p1, *p2, *p3;
  while (get ())
    {
      switch (get ())
	{
	default:
	case 0:
	  p0 = malloc (16);
	  break;
	case 1:
	  free (p0); /* { dg-warning "double-'free' of 'p0'" } */
	  break;

	case 2:
	  p1 = malloc (16);
	  break;
	case 3:
	  free (p1); /* { dg-warning "double-'free' of 'p1'" } */
	  break;

	case 4:
	  p2 = malloc (16);
	  break;
	case 5:
	  free (p2); /* { dg-warning "double-'free' of 'p2'" } */
	  break;

	case 6:
	  p3 = malloc (16);
	  break;
	case 7:
	  free (p3); /* { dg-warning "double-'free' of 'p3'" } */
	  break;
	}
    }
}
