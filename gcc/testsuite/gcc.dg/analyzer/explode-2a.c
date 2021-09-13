/* PR analyzer/101068.  */

/* { dg-additional-options "--param analyzer-max-enodes-per-program-point=200 --param analyzer-bb-explosion-factor=50" } */

#include <stdlib.h>

extern int get (void);

void test (void)
{
  void *p0 = NULL, *p1 = NULL, *p2 = NULL, *p3 = NULL;
  /* Due to not purging constraints on SSA names within loops
     (PR analyzer/101068), the analyzer effectively treats the original
     explode-2.c as this code.  */
  int a = get ();
  int b = get ();
  while (a)
    {
      switch (b)
	{
	default:
	case 0:
	  p0 = malloc (16); /* { dg-warning "leak" } */
	  break;
	case 1:
	  free (p0); /* { dg-warning "double-'free' of 'p0'" "" { xfail *-*-* } } */
	  break;

	case 2:
	  p1 = malloc (16); /* { dg-warning "leak" } */
	  break;
	case 3:
	  free (p1); /* { dg-warning "double-'free' of 'p1'" "" { xfail *-*-* } } */
	  break;

	case 4:
	  p2 = malloc (16); /* { dg-warning "leak" } */
	  break;
	case 5:
	  free (p2); /* { dg-warning "double-'free' of 'p2'" "" { xfail *-*-* } } */
	  break;

	case 6:
	  p3 = malloc (16); /* { dg-warning "leak" } */
	  break;
	case 7:
	  free (p3); /* { dg-warning "double-'free' of 'p3'" "" { xfail *-*-* } } */
	  break;
	}
    }
}
