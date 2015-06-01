/* { dg-do compile } */

unsigned int a;
int b, c, d, e, f;

void
fn1 ()
{
  for (; b; b++)
    {
      for (e = 0; e < 1; e++)
	;
      int i = -1, j = 1;
      if (f)
	{
	  for (; a < 1; a++)
	    ;
	  i++;
	  j = 0;
	}
      for (; d; d++)
	{
	  if (j)
	    for (;; c++)
	      ;
	  if (i)
	    continue;
	  return;
	}
    }
}
