/* PR opt/13159 -- test unswitching a loop multiple times.  */

void
foo (void)
{
  long j, k, p, g;

  while (p)
    {
      while (k < 0 && j < 0)
	;
      if (g)
	;
      else if (g)
	;      
    }
}
