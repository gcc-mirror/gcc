/* { dg-do compile } */

int printf (const char *, ...);

int a, f, g;
char b, d;
short c;
static short e;

char
fn1 ()
{
  for (; b; b++)
    {
      int h = 5;
      for (a = 0; a < 1; a++)
	{
	  for (d = 0; d < 1; d++)
	    for (c = 0; c < 1; c++)
	      for (; e >= 0;)
		return 5;
	  if (f)
	    h = 0;
	}
      if (h)
	printf ("%d", 0);
    }
  return g;
}
