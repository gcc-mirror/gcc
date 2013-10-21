/* { dg-do run } */
/* { dg-additional-options "-fstrict-overflow" } */

extern void abort (void);

int a, b, c, d, e, f, g, h = 1, i;

int foo (int p)
{
  return p < 0 && a < - __INT_MAX__ - 1 - p ? 0 : 1;
}

int *bar ()
{
  int j; 
  i = h ? 0 : 1 % h;
  for (j = 0; j < 1; j++)
    for (d = 0; d; d++)
      for (e = 1; e;)
	return 0;
  return 0;
}

int baz ()
{
  for (; b >= 0; b--)
    for (c = 1; c >= 0; c--)
      {
	int *k = &c;
	for (;;)
	  {
	    for (f = 0; f < 1; f++)
	      {
		g = foo (*k);
		bar ();
	      }
	    if (*k)
	      break;
	    return 0;
	  }
      }
  return 0;
}

int main ()
{
  baz ();
  if (b != 0)
    abort ();
  return 0;
}
