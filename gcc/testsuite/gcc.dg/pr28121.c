/* { dg-do compile } */
/* { dg-options "-O2" } */

short
GetCmd ()
{
  int c, c1;
  for (c = 255; c == 255;)
    {
      c = GetMouseButton ();
      if (c >= 0)
	{
	  c = ParsePos (c, -1, 0);
	  c1 = ParsePos (c1, c, 1);
	  if (c == c1 && (c >= 0 || c == -10))
	    {
	      return c;
	    }
	  if (c >= 0 && c1 == -12)
	    {
	      return ((((((10) + 1) + 1)) + 1) << 7) | c;
	    }
	  c = 255;
	}
    }
}

