// { dg-do compile }
// { dg-options -O2 }

int foo ();
double bar (void)
{
  union
  {
    char a[8];
    double b;
  };

  a[0] = foo ();
  a[1] = foo ();
  a[2] = foo ();
  a[3] = foo ();
  a[4] = foo ();
  a[5] = foo ();
  a[6] = foo ();
  a[7] = foo ();
  return b;
}
