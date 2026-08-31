/* { dg-do run } */
/* { dg-options "-O1" } */

signed char a;
int b, c;
short d[1];

short
foo (short *f, short g)
{
  int i = 0, j, l;
  long n = 1;
  for (int m = 0; m < a; m++)
    for (; i <= m; i++)
      n = i;
  for (unsigned k = 0; k < g; k++)
    {
      l = 0;
      do
	if (f)
	  j = l + a;
      while (++l <= k);
    }
  n ^= j;
  while (a)
    n ^= b;
  return n;
}

int
main ()
{
  if (foo (d, 2) != 0)
    __builtin_abort ();
}
