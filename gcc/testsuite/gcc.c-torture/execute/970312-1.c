#include <stdio.h>

__inline__ static int
dummy (x)
{
  int y;
  y = (long) (x * 4711.3);
  return y;
}

int getval (void);

int
f2 (double x)
{
  unsigned short s;
  int a, b, c, d, e, f, g, h, i, j;

  a = getval ();
  b = getval ();
  c = getval ();
  d = getval ();
  e = getval ();
  f = getval ();
  g = getval ();
  h = getval ();
  i = getval ();
  j = getval ();


  s = x;

  return a + b + c + d + e + f + g + h + i + j + s;
}

int x = 1;

int
getval (void)
{
  return x++;
}

char buf[10];

void
f ()
{
  int a, b, c, d, e, f, g, h, i, j, k;

  a = getval ();
  b = getval ();
  c = getval ();
  d = getval ();
  e = getval ();
  f = getval ();
  g = getval ();
  h = getval ();
  i = getval ();
  j = getval ();

  k = f2 (17.0);

  sprintf (buf, "%d\n", a + b + c + d + e + f + g + h + i + j + k);
  if (a + b + c + d + e + f + g + h + i + j + k != 227)
    abort ();
}

main ()
{
  f ();
  exit (0);
}
