double a[4], b[2];

void __attribute__((noipa))
foo ()
{
  double a0 = a[0];
  double a1 = a[1];
  double a2 = a[2];
  double a3 = a[3];
  b[0] = a1 - a3;
  b[1] = a0 + a2;
}

int main()
{
  a[0] = 1.;
  a[1] = 2.;
  a[2] = 3.;
  a[3] = 4.;
  foo ();
  if (b[0] != -2 || b[1] != 4)
    __builtin_abort ();
  return 0;
}
