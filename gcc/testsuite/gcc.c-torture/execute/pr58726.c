/* PR rtl-optimization/58726 */

int a, c;
union { int f1; int f2 : 1; } b;

short
foo (short p)
{
  return p < 0 ? p : a;
}

int
main ()
{
  if (sizeof (short) * __CHAR_BIT__ != 16
      || sizeof (int) * __CHAR_BIT__ != 32)
    return 0;
  b.f1 = 56374;
  unsigned short d;
  int e = b.f2;
  d = e == 0 ? b.f1 : 0;
  c = foo (d);
  if (c != (short) 56374)
    __builtin_abort ();
  return 0;
}
