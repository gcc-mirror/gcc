__attribute__ ((noinline))
foo(short unsigned int *p1, short unsigned int *p2)
{
  short unsigned int x1, x4;
  int x2, x3, x5, x6;
  unsigned int x7;
  
  x1 = *p1;
  x2 = (int) x1;
  x3 = x2 * 65536;
  x4 = *p2;
  x5 = (int) x4;
  x6 = x3 + x4;
  x7 = (unsigned int) x6;
  if (x7 <= 268435455U)
    abort ();
  exit (0);
}

main()
{
  short unsigned int x, y;
  x = -5;
  y = -10;
  foo (&x, &y);
}

