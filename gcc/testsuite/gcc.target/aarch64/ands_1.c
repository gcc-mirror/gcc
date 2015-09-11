/* { dg-do run } */
/* { dg-options "-O2 --save-temps -fno-inline" } */

extern void abort (void);

int
ands_si_test1 (int a, int b, int c)
{
  int d = a & b;

  /* { dg-final { scan-assembler-times "ands\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+" 2 } } */
  if (d == 0)
    return a + c;
  else
    return d;
}

int
ands_si_test2 (int a, int b, int c)
{
  int d = a & 0xff;

  /* { dg-final { scan-assembler "ands\tw\[0-9\]+, w\[0-9\]+, 255" } } */
  if (d == 0)
    return a + c;
  else
    return d;
}

int
ands_si_test3 (int a, int b, int c)
{
  int d = a & (b << 3);

  /* { dg-final { scan-assembler "ands\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+, lsl 3" } } */
  if (d == 0)
    return a + c;
  else
    return d;
}

typedef long long s64;

s64
ands_di_test1 (s64 a, s64 b, s64 c)
{
  s64 d = a & b;

  /* { dg-final { scan-assembler-times "ands\tx\[0-9\]+, x\[0-9\]+, x\[0-9\]+" 2 } } */
  if (d == 0)
    return a + c;
  else
    return d;
}

s64
ands_di_test2 (s64 a, s64 b, s64 c)
{
  s64 d = a & 0xff;

  /* { dg-final { scan-assembler "ands\tx\[0-9\]+, x\[0-9\]+, 255" } } */
  if (d == 0)
    return a + c;
  else
    return d;
}

s64
ands_di_test3 (s64 a, s64 b, s64 c)
{
  s64 d = a & (b << 3);

  /* { dg-final { scan-assembler "ands\tx\[0-9\]+, x\[0-9\]+, x\[0-9\]+, lsl 3" } } */
  if (d == 0)
    return a + c;
  else
    return d;
}

int
main ()
{
  int x;
  s64 y;

  x = ands_si_test1 (29, 4, 5);
  if (x != (29 & 4))
    abort ();

  x = ands_si_test1 (5, 2, 20);
  if (x != 25)
    abort ();

  x = ands_si_test2 (29, 4, 5);
  if (x != (29 & 0xff))
    abort ();

  x = ands_si_test2 (1024, 2, 20);
  if (x != 1044)
    abort ();

  x = ands_si_test3 (35, 4, 5);
  if (x != (35 & (4 << 3)))
    abort ();

  x = ands_si_test3 (5, 2, 20);
  if (x != 25)
    abort ();

  y = ands_di_test1 (0x130000029ll,
                     0x320000004ll,
                     0x505050505ll);

  if (y != ((0x130000029ll & 0x320000004ll)))
    abort ();

  y = ands_di_test1 (0x5000500050005ll,
                     0x2111211121112ll,
                     0x0000000002020ll);
  if (y != 0x5000500052025ll)
    abort ();

  y = ands_di_test2 (0x130000029ll,
                     0x320000004ll,
                     0x505050505ll);
  if (y != ((0x130000029ll & 0xff)))
    abort ();

  y = ands_di_test2 (0x130002900ll,
                     0x320000004ll,
                     0x505050505ll);
  if (y != (0x130002900ll + 0x505050505ll))
    abort ();

  y = ands_di_test3 (0x130000029ll,
                     0x064000008ll,
                     0x505050505ll);
  if (y != ((0x130000029ll & (0x064000008ll << 3))))
    abort ();

  y = ands_di_test3 (0x130002900ll,
                     0x088000008ll,
                     0x505050505ll);
  if (y != (0x130002900ll + 0x505050505ll))
    abort ();

  return 0;
}

