/* { dg-do compile } */
/* { dg-options "-O3 -g" } */

char a, b, i, j;
int c, d, e, f, g, h, n;

char
fn1 ()
{
  char k, l, m;
  int p;
  e = g > f;
  for (b = 0; b < 2; b++)
    {
      for (p = 0; p < 3; p++)
	{
	  for (; h < 1; h++)
	    {
	      for (; m;)
		goto lbl;
	      e = g;
	    }
	  l = a < 0 || a < d;
	}
      d++;
      for (;;)
	{
	  k = g;
	  n = -k;
	  j = n;
	  c = j;
	  e = 2;
	  if (l)
	    break;
	  return 2;
	}
    }
  for (;;)
    ;
 lbl:
  return i;
}
