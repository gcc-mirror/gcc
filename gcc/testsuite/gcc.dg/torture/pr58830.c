/* { dg-do run } */
/* { dg-additional-options "-ftree-pre -ftree-partial-pre" } */

extern void abort (void);

int b, c, d, f, g, h, i, j[6], *l = &b, *m, n, *o, r; 
char k;

static int
foo ()
{
  char *p = &k;

  for (; d; d++)
    if (i)
      h = 0;
    else
      h = c || (r = 0);

  for (f = 0; f < 2; f++)
    {
      unsigned int q;
      *l = 0;
      if (n)
	*m = g;
      if (g)
	o = 0;
      for (q = -8; q >= 5; q++)
	(*p)--;
    }

  return 0;
}

int
main ()
{
  foo ();
  if (j[j[j[j[j[j[j[j[j[j[j[j[j[j[j[j[j[j[j[j[j[j[j[j[j[j[j[j[j[j[j[j[j[j[j[j[j[j[j[j[j[j[0]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]] ^ (k & 15)] != 0)
    abort ();
  return 0;
}
