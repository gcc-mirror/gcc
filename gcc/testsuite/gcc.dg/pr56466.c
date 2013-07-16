/* PR rtl-optimization/56466 */
/* { dg-do compile } */
/* { dg-options "-O2 -w -funroll-loops" } */

int a, b, c;

void
f (void)
{
  for (; b; b++)
    {
      if (0)
	for (; b < 0; b++)
	  if (1 % 0)
	    {
	      while (1)
		{
		  a = 0;
		lbl1:
		  c++;
		}
	    lbl2:
	      ;
	    }

      goto lbl1;
    }

  a = 0;
  goto lbl2;
}
