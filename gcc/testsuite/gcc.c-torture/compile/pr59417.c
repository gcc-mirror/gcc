/* PR tree-optimization/59417 */

int a, b, d;
short c;

void
f (void)
{
  if (b)
    {
      int *e;

      if (d)
	{
	  for (; b; a++)
	  lbl1:
	    d = 0;

	  for (; d <= 1; d++)
	    {
	      int **q = &e;
	      for (**q = 0; **q <= 0; **q++)
		d = 0;
	    }
	}
    }

  else
    {
      int t;
      for (c = 0; c < 77; c++)
	for (c = 0; c < 46; c++);
      for (; t <= 0; t++)
      lbl2:
	;
      goto lbl1;
    }
  goto lbl2;
}
