/* { dg-xfail-if "ptxas crashes" { nvptx-*-* } { "-O0" } { "" } } */

int a, b, d = 1, e, f, o, u, w = 1, z;
short c, q, t;

int
main ()
{
  char g;
  for (; d; d--)
    {
      while (o)
	for (; e;)
	  {
	    c = b;
	    int h = o = z;
	    for (; u;)
	      for (; a;)
		;
	  }
      if (t < 1)
	g = w;
      f = g;
      g && (q = 1);
    }

  if (q != 1)
    __builtin_abort ();

  return 0;
}
