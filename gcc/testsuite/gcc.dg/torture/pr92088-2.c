/* { dg-do compile } */
/* { dg-require-effective-target alloca } */

void foo(int n)
{
  struct X { int a[n]; } y;

  struct X baz (struct X x)
    {
      x.a[0] = 1;
      return x;
    }

  y.a[0] = 0;
  y = baz(y);
  if (y.a[0] != 1)
    __builtin_abort ();
}
