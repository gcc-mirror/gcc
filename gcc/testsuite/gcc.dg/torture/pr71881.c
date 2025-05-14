/* { dg-do compile } */
/* { dg-additional-options "-g" } */

int a, b, c, d, *e, f, g;

int fn1 ()
{
  char h[2];
  int i = 0;
  for (; i < 2; i++)
    {
      if (c)
	for (*e = 0; *e;)
	  {
	    int j[f];
	    i = *e;
	  }
      h[i] = 0;
    }
  for (; a;)
    return h[0];
  for (b = 0; b;)
    i = g = (1 & i) < d;
  return 0;
}
