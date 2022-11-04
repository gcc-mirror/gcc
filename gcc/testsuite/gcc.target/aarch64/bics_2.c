/* { dg-do run } */
/* { dg-options "-O2 --save-temps -fno-inline" } */

extern void abort (void);

int
bics_si_test1 (int a, int b, int c)
{
  int d = a & ~b;

  /* { dg-final { scan-assembler "bics\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+" } } */
  if (d <= 0)
    return a + c;
  else
    return b + d + c;
}

int
bics_si_test2 (int a, int b, int c)
{
  int d = a & ~(b << 3);

  /* { dg-final { scan-assembler "bics\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+, lsl 3" } } */
  if (d > 0)
    return b + d + c;
  else
    return a + c;
}

typedef long long s64;

s64
bics_di_test1 (s64 a, s64 b, s64 c)
{
  s64 d = a & ~b;

  /* { dg-final { scan-assembler "bics\tx\[0-9\]+, x\[0-9\]+, x\[0-9\]+" } } */
  if (d <= 0)
    return a + c;
  else
    return b + d + c;
}

s64
bics_di_test2 (s64 a, s64 b, s64 c)
{
  s64 d = a & ~(b << 3);

  /* { dg-final { scan-assembler "bics\tx\[0-9\]+, x\[0-9\]+, x\[0-9\]+, lsl 3" } } */
  if (d > 0)
    return b + d + c;
  else
    return a + c;
}

int
main ()
{
  int x;
  s64 y;

  x = bics_si_test1 (29, ~4, 5);
  if (x != ((29 & 4) + ~4 + 5))
    abort ();

  x = bics_si_test1 (5, ~2, 20);
  if (x != 25)
    abort ();

  x = bics_si_test2 (35, ~4, 5);
  if (x != ((35 & ~(~4 << 3)) + ~4 + 5))
    abort ();

  x = bics_si_test2 (96, ~2, 20);
  if (x != 116)
    abort ();

  y = bics_di_test1 (0x130000029ll,
                     ~0x320000004ll,
                     0x505050505ll);

  if (y != ((0x130000029ll & 0x320000004ll) + ~0x320000004ll + 0x505050505ll))
    abort ();

  y = bics_di_test1 (0x5000500050005ll,
                     ~0x2111211121112ll,
                     0x0000000002020ll);
  if (y != 0x5000500052025ll)
    abort ();

  y = bics_di_test2 (0x130000029ll,
                     ~0x064000008ll,
                     0x505050505ll);
  if (y != ((0x130000029ll & ~(~0x064000008ll << 3))
	    + ~0x064000008ll + 0x505050505ll))
    abort ();

  y = bics_di_test2 (0x130002900ll,
                     ~0x088000008ll,
                     0x505050505ll);
  if (y != (0x130002900ll + 0x505050505ll))
    abort ();

  return 0;
}

