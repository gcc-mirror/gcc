float a[8], b[4];

void __attribute__((noipa))
foo ()
{
  float a0 = a[0];
  float a1 = a[1];
  float a2 = a[2];
  float a3 = a[3];
  float a4 = a[4];
  float a5 = a[5];
  float a6 = a[6];
  float a7 = a[7];
  b[0] = a1 - a5;
  b[1] = a0 + a4;
  b[2] = a3 - a7;
  b[3] = a2 + a6;
}

int main()
{
  a[0] = 1.;
  a[1] = 2.;
  a[2] = 3.;
  a[3] = 4.;
  a[4] = 5.;
  a[5] = 6.;
  a[6] = 7.;
  a[7] = 8.;
  foo ();
  if (b[0] != -4 || b[1] != 6 || b[2] != -4 || b[3] != 10)
    __builtin_abort ();
  return 0;
}
