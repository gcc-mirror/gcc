/* { dg-additional-options "-std=gnu89" } */

long f (w)
     char *w;
{
  long k, i, c = 0, x;
  char *p = (char*) &x;
  for (i = 0; i < 1; i++)
    {
      for (k = 0; k < sizeof (long); k++)
	p[k] = w[k];
      c += x;
    }
  return c;
}

main ()
{
  int i;
  char a[sizeof (long)];

  for (i = sizeof (long); --i >= 0;) a[i] = ' ';
  if (f (a) != ~0UL / (unsigned char) ~0 * ' ')
    abort ();
  exit (0);
}
