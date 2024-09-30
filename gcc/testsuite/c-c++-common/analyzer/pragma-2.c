/* { dg-skip-if "" { powerpc*-*-aix* } } */
/* Verify that we can disable -Wanalyzer-too-complex via pragmas.  */
/* { dg-additional-options "-Wanalyzer-too-complex -Werror=analyzer-too-complex -fno-analyzer-state-merge -g" } */
/* { dg-skip-if "requires hosted libstdc++ for stdlib malloc" { ! hostedlib } } */

#include <stdlib.h>

extern int get (void);

/* In theory each of p0...p4 can be in various malloc states,
   independently, so the total combined number of states
   at any program point within the loop is NUM_VARS * NUM_STATES.  */

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wanalyzer-too-complex"

void test (void)
{
  void *p0 = NULL, *p1 = NULL, *p2 = NULL, *p3 = NULL, *p4 = NULL;
  void **pp = NULL;
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
	}

      switch (get ())
	{
	default:
	case 0:
	  *pp = malloc (16); /* { dg-warning "leak" } */
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

#pragma GCC diagnostic pop
