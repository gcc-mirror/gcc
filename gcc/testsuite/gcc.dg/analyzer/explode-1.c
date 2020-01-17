/* { dg-additional-options "-Wno-analyzer-too-complex" } */

#include <stdlib.h>

extern int get (void);

/* In theory each of p0...p8 can be in various malloc states,
   independently, so the total combined number of states
   at any program point within the loop is NUM_VARS * NUM_STATES.  */

void test (void)
{
  void *p0, *p1, *p2, *p3, *p4, *p5, *p6, *p7, *p8;
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
	case 2:
	  pp = &p2;
	  break;
	case 3:
	  pp = &p3;
	  break;
	case 4:
	  pp = &p4;
	  break;
	case 5:
	  pp = &p5;
	  break;
	case 6:
	  pp = &p6;
	  break;
	case 7:
	  pp = &p7;
	  break;
	}

      switch (get ())
	{
	default:
	case 0:
	  *pp = malloc (16);
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
