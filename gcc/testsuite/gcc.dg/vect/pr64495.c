#include <assert.h>
#include "tree-vect.h"

int a, b, c, d, e, f, g, i, j;
static int *h = &e;

int
main ()
{
  check_vect ();

  for (; a;)
    for (; g; g++)
#pragma GCC novector
      for (; f; f++)
	if (j)
	  {
	    assert(b); 
	    assert(0);
	  }
  for (i = 24; i; i--)
    {
      for (c = 0; c < 6; c++)
	d |= 1;
      *h |= d;
    }

  if (e != 1) 
    __builtin_abort (); 

  return 0;
}

