/* In theory each of p0...p3 can be in various malloc states,
   independently, so the total combined number of states
   at any program point within the loop is NUM_VARS * NUM_STATES.

   However, due to the way the analyzer represents heap-allocated regions
   this never terminates, eventually hitting the complexity limit
   (PR analyzer/93695).  */

/* { dg-additional-options "-Wno-analyzer-too-complex -Wno-analyzer-malloc-leak" } */

#include <stdlib.h>

extern int get (void);

void test (void)
{
  void *p0 = NULL, *p1 = NULL, *p2 = NULL, *p3 = NULL;
  while (get ())
    {
      switch (get ())
	{
	default:
	case 0:
	  p0 = malloc (16); /* { dg-warning "leak" "" { xfail *-*-* } } */
	  break;
	case 1:
	  free (p0); /* { dg-warning "double-'free' of 'p0'" } */
	  break;

	case 2:
	  p1 = malloc (16); /* { dg-warning "leak" "" { xfail *-*-* } } */
	  break;
	case 3:
	  free (p1); /* { dg-warning "double-'free' of 'p1'" "" { xfail *-*-* } } */
	  break;

	case 4:
	  p2 = malloc (16); /* { dg-warning "leak" "" { xfail *-*-* } } */
	  break;
	case 5:
	  free (p2); /* { dg-warning "double-'free' of 'p2'" "" { xfail *-*-* } } */
	  break;

	case 6:
	  p3 = malloc (16); /* { dg-warning "leak" "" { xfail *-*-* } } */
	  break;
	case 7:
	  free (p3); /* { dg-warning "double-'free' of 'p3'" "" { xfail *-*-* } } */
	  break;
	}
    }
}
