#include "tree-vect.h"

int a, b, c, d, e, f, g, h;

int
main ()
{
  check_vect ();

  for (; a; a--)
    for (d = 1; d <= 0; d++)
#pragma GCC novector
      for (; d;)
	if (h)
	  {
	    if (!g) __builtin_abort ();
	    if (!0) __builtin_abort ();
	  }

  for (f = 4; f; f--)
    {
      for (b = 0; b < 2; b++)
	c |= 1;
      e |= c;
    }

  return 0;
}

