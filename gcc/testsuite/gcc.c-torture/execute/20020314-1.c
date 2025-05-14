/* { dg-additional-options "-fpermissive" } */
void f(void * a, double y)
{
}

double g (double a, double b, double c, double d)
{
  double x, y, z;
  void *p;

  x = a + b;
  y = c * d;

  p = alloca (16);

  f(p, y);
  z = x * y * a;

  return z + b;
}

main ()
{
  double a, b, c, d;
  a = 1.0;
  b = 0.0;
  c = 10.0;
  d = 0.0;

  if (g (a, b, c, d) != 0.0)
    abort ();

  if (a != 1.0 || b != 0.0 || c != 10.0 || d != 0.0)
    abort ();

  exit (0);
}
