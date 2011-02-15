/* { dg-do compile } */

int
foo (void *x, int y)
{
  long long a = 1, *b;
  double *c;
  if (y)
    {
      b = (long long *) x;
      while (b)
	a *= *b++;
    }
  else
    {
      c = (double *) x;
      while (c)
	a *= *c++;
    }
  return a;
}

