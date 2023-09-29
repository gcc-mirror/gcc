double a[2], b[2], c[2], d[2];

void __attribute__((noipa))
foo()
{
  double a0 = a[0];
  double a1 = a[1];
  double b0 = b[0];
  double b1 = b[1];
  double c0 = c[0];
  double c1 = c[1];
  double tem1 = a1 - b1;
  double tem2 = a0 + b0;
  d[0] = tem1 * c1;
  d[1] = tem2 * c0;
}

int main()
{
  a[0] = 1.;
  a[1] = 2.;
  b[0] = 3.;
  b[1] = 4.;
  c[0] = 2.;
  c[1] = 3.;
  foo ();
  if (d[0] != -6. || d[1] != 8.)
    __builtin_abort ();
  return 0;
}
