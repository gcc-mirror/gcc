/* PR tree-optimization/56035 */
/* { dg-do compile } */
/* { dg-options "-O1 -ftree-vectorize -fcse-follow-jumps -fstrict-overflow" } */

short a, c, *p;

void
f (void)
{
  int b;

  if (c)
  lbl1:
    for (a = 0; a < 1; a++)
      {
	for (c = 0; c < 1; c++)
	  {
	    goto lbl1;
	    while (*p++)
	    lbl2:
	      ;
	  }
      }

  for (;; b++)
    {
      if (c)
	goto lbl2;
    lbl3:
      for (c = 0; c < 9; c++)
	for (c = -17; c < 2; c++)
	  if (*p)
	    goto lbl3;
    }
}
