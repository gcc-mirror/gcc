/* { dg-do run } */
/* { dg-options "-O2" } */

/* Verifies that the potential CC clobber is taken into account when expanding
   doloop_end but this fails to resolve into a hardware loop.  */

int d = 1, e, f, o, w = 1, q, t;

int
main (void)
{
  int g = 0;

  for (; d; d--)
    {
      while (o)
	while (e)
	  ;

      g = t ? g : w;
      f = g;
      if (g)
	q = 1;
    }

  if (q != 1)
    __builtin_abort ();

  return 0;
}
