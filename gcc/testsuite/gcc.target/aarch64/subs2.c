/* { dg-do run } */
/* { dg-options "-O2 --save-temps -fno-inline" } */

extern void abort (void);

int
subs_si_test1 (int a, int b, int c)
{
  int d = a - b;

  /* { dg-final { scan-assembler-not "subs\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+" } } */
  /* { dg-final { scan-assembler "sub\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+" } } */
  if (d <= 0)
    return a + c;
  else
    return b + d + c;
}

int
subs_si_test2 (int a, int b, int c)
{
  int d = a - 0xfff;

  /* { dg-final { scan-assembler-not "subs\tw\[0-9\]+, w\[0-9\]+, #4095" } } */
  /* { dg-final { scan-assembler "sub\tw\[0-9\]+, w\[0-9\]+, #4095" } } */
  if (d <= 0)
    return a + c;
  else
    return b + d + c;
}

int
subs_si_test3 (int a, int b, int c)
{
  int d = a - (b << 3);

  /* { dg-final { scan-assembler-not "subs\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+, lsl 3" } } */
  /* { dg-final { scan-assembler "sub\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+, lsl 3" } } */
  if (d <= 0)
    return a + c;
  else
    return b + d + c;
}

typedef long long s64;

s64
subs_di_test1 (s64 a, s64 b, s64 c)
{
  s64 d = a - b;

  /* { dg-final { scan-assembler-not "subs\tx\[0-9\]+, x\[0-9\]+, x\[0-9\]+" } } */
  /* { dg-final { scan-assembler "sub\tx\[0-9\]+, x\[0-9\]+, x\[0-9\]+" } } */
  if (d <= 0)
    return a + c;
  else
    return b + d + c;
}

s64
subs_di_test2 (s64 a, s64 b, s64 c)
{
  s64 d = a - 0x1000ll;

  /* { dg-final { scan-assembler-not "subs\tx\[0-9\]+, x\[0-9\]+, #4096" } } */
  /* { dg-final { scan-assembler "sub\tx\[0-9\]+, x\[0-9\]+, #4096" } } */
  if (d <= 0)
    return a + c;
  else
    return b + d + c;
}

s64
subs_di_test3 (s64 a, s64 b, s64 c)
{
  s64 d = a - (b << 3);

  /* { dg-final { scan-assembler-not "subs\tx\[0-9\]+, x\[0-9\]+, x\[0-9\]+, lsl 3" } } */
  /* { dg-final { scan-assembler "sub\tx\[0-9\]+, x\[0-9\]+, x\[0-9\]+, lsl 3" } } */
  if (d <= 0)
    return a + c;
  else
    return b + d + c;
}

int main ()
{
  int x;
  s64 y;

  x = subs_si_test1 (29, 4, 5);
  if (x != 34)
    abort ();

  x = subs_si_test1 (5, 2, 20);
  if (x != 25)
    abort ();

  x = subs_si_test2 (29, 4, 5);
  if (x != 34)
    abort ();

  x = subs_si_test2 (1024, 2, 20);
  if (x != 1044)
    abort ();

  x = subs_si_test3 (35, 4, 5);
  if (x != 12)
    abort ();

  x = subs_si_test3 (5, 2, 20);
  if (x != 25)
    abort ();

  y = subs_di_test1 (0x130000029ll,
		     0x320000004ll,
		     0x505050505ll);

  if (y != 0x63505052e)
    abort ();

  y = subs_di_test1 (0x5000500050005ll,
		     0x2111211121112ll,
		     0x0000000002020ll);
  if (y != 0x5000500052025)
    abort ();

  y = subs_di_test2 (0x130000029ll,
		     0x320000004ll,
		     0x505050505ll);
  if (y != 0x95504f532)
    abort ();

  y = subs_di_test2 (0x540004100ll,
		     0x320000004ll,
		     0x805050205ll);
  if (y != 0x1065053309)
    abort ();

  y = subs_di_test3 (0x130000029ll,
		     0x064000008ll,
		     0x505050505ll);
  if (y != 0x63505052e)
    abort ();

  y = subs_di_test3 (0x130002900ll,
		     0x088000008ll,
		     0x505050505ll);
  if (y != 0x635052e05)
    abort ();

  return 0;
}

/* { dg-final { cleanup-saved-temps } } */
