/* { dg-do run } */
/* { dg-options "-O2 --save-temps -fno-inline" } */

extern void abort (void);

int
bics_si_test1 (int a, int b, int c)
{
  if ((a & b) == a)
    return a;
  else
    return c;
}

int
bics_si_test2 (int a, int b, int c)
{
  if ((a & b) == b)
    return b;
  else
    return c;
}

typedef long long s64;

s64
bics_di_test1 (s64 a, s64 b, s64 c)
{
  if ((a & b) == a)
    return a;
  else
    return c;
}

s64
bics_di_test2 (s64 a, s64 b, s64 c)
{
  if ((a & b) == b)
    return b;
  else
    return c;
}

int
main ()
{
  int x;
  s64 y;

  x = bics_si_test1 (0xf00d, 0xf11f, 0);
  if (x != 0xf00d)
    abort ();

  x = bics_si_test1 (0xf11f, 0xf00d, 0);
  if (x != 0)
    abort ();

  x = bics_si_test2 (0xf00d, 0xf11f, 0);
  if (x != 0)
    abort ();

  x = bics_si_test2 (0xf11f, 0xf00d, 0);
  if (x != 0xf00d)
    abort ();

  y = bics_di_test1 (0x10001000f00dll, 0x12341000f00dll, 0ll);
  if (y != 0x10001000f00dll)
    abort ();

  y = bics_di_test1 (0x12341000f00dll, 0x10001000f00dll, 0ll);
  if (y != 0)
    abort ();

  y = bics_di_test2 (0x10001000f00dll, 0x12341000f00dll, 0ll);
  if (y != 0)
    abort ();

  y = bics_di_test2 (0x12341000f00dll, 0x10001000f00dll, 0ll);
  if (y != 0x10001000f00dll)
    abort ();

  return 0;
}

/* { dg-final { scan-assembler-times "bics\twzr, w\[0-9\]+, w\[0-9\]+" 2 } } */
/* { dg-final { scan-assembler-times "bics\txzr, x\[0-9\]+, x\[0-9\]+" 2 } } */
