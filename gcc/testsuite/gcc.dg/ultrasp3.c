/* { dg-options "" } */
/* { dg-options "-mcpu=ultrasparc -mv8plus" { target sparc-*-* } } */

unsigned long long foo (unsigned long long x)
{
  return 0x73500000735LL * x;
}

unsigned long long a, b;
unsigned long p;

unsigned long long bar (void)  
{
  unsigned long long c = a | b;
  return 0x73500000735LL * c;
}

unsigned long long baz (void)
{
  unsigned long long c = (p + 345) & -2;
  return c * a;
}

main ()
{
  if (foo (0x56789LL) != 0x26f32e5d26f32e5dLL)
    abort ();
  a = 0x8000000080000000LL;
  b = 0x0000000180000001LL;
  if (bar () != 0x120480000735LL)
    abort ();
  p = 0xffffffff;
  if (baz () != 0xac00000000LL)
    abort ();
  exit (0);
}
