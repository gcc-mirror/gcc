/* { dg-additional-options "-std=gnu89" } */

int
foo (a, b)
int *a,  *b;
{
  int x, y;
  x++;
  *a = *b;
  y = *b;

  if ((int) x)
    return 1;
  else
    return y;
}

foo1 (p)
     int *p;
{
  p[0] = p[1];
  return p[0];
}

foo2 (p, x)
     int *p;
{
  p[0] = x;
  return p[0];
}

