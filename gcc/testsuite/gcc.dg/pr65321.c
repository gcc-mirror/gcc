/* PR rtl-optimization/65321 */
/* { dg-do compile } */
/* { dg-options "-O3 -g" } */

int a, b, c, d, e;

int
foo (void)
{
  int h;
  char i;
  for (; c > 0;)
    {
      for (d = 0; d < 2; d++)
	{
	  i = 1 << d;
	  if (i - a)
	    {
	      e = b = 0;
	      for (; c; c--)
		d = 127;
	    }
	}
      h = ~d;
      if (h > c)
	for (;;)
	  ;
      return 0;
    }
  return 0;
}
