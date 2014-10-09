/* { dg-do run } */

int a = 0, b = 1, c = 0, d = 1, e, f, g, h;
int
main ()
{
  e = 1 >> d;
  f = ((31 / (1 > e)) || c) / 2;
  g = b || a;
  h = 31 / g;
  if (!h)
    __builtin_abort();
  return 0;
}

