#include "tree-vect.h"

int a, b, c, e, f;

void
foo ()
{
  for (b = 0; b < 3; b++)
    if (e)
      {
	for (c = 0; c < 4; c++)
	  {
	    if (b)
	      continue;
	    f = 1;
	    for (a = 0; a < 2; a++)
	      f |= 1;
	  }
	for (;;)
	  ;
      }
}

int
main ()
{
  check_vect ();
  foo ();
  return 0;
}

/* { dg-final { cleanup-tree-dump "vect" } } */
