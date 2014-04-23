/* { dg-do compile } */

extern int a, b, k, q;

void
foo ()
{
  if (a)
    {
      while (q)
	{
	lbl:
	  if (a)
	    {
	      a = 0;
	      goto lbl;
	    }
	}
      b = k;
    }
  goto lbl;
}
