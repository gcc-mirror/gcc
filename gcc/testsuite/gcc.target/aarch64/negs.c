/* { dg-do run } */
/* { dg-options "-O2 --save-temps" } */

extern void abort (void);
int z;

int
negs_si_test1 (int a, int b, int c)
{
  int d = -b;

  /* { dg-final { scan-assembler "negs\tw\[0-9\]+, w\[0-9\]+" } } */
  if (d < 0)
    return a + c;

  z = d;
    return b + c + d;
}

int
negs_si_test3 (int a, int b, int c)
{
  int d = -(b) << 3;

  /* { dg-final { scan-assembler "negs\tw\[0-9\]+, w\[0-9\]+, lsl 3" } } */
  if (d == 0)
    return a + c;

  z = d;
    return b + c + d;
}

typedef long long s64;
s64 zz;

s64
negs_di_test1 (s64 a, s64 b, s64 c)
{
  s64 d = -b;

  /* { dg-final { scan-assembler "negs\tx\[0-9\]+, x\[0-9\]+" } } */
  if (d < 0)
    return a + c;

  zz = d;
    return b + c + d;
}

s64
negs_di_test3 (s64 a, s64 b, s64 c)
{
  s64 d = -(b) << 3;

  /* { dg-final { scan-assembler "negs\tx\[0-9\]+, x\[0-9\]+, lsl 3" } } */
  if (d == 0)
    return a + c;

  zz = d;
    return b + c + d;
}

int main ()
{
  int x;
  s64 y;

  x = negs_si_test1 (2, 12, 5);
  if (x != 7)
    abort ();

  x = negs_si_test1 (1, 2, 32);
  if (x != 33)
    abort ();

  x = negs_si_test3 (13, 14, 5);
  if (x != -93)
    abort ();

  x = negs_si_test3 (15, 21, 2);
  if (x != -145)
    abort ();

  y = negs_di_test1 (0x20202020ll,
		     0x65161611ll,
		     0x42434243ll);
  if (y != 0x62636263ll)
    abort ();

  y = negs_di_test1 (0x1010101010101ll,
		     0x123456789abcdll,
		     0x5555555555555ll);
  if (y != 0x6565656565656ll)
    abort ();

  y = negs_di_test3 (0x62523781ll,
		     0x64234978ll,
		     0x12345123ll);
  if (y != 0xfffffffd553d4edbll)
    abort ();

  y = negs_di_test3 (0x763526268ll,
		     0x101010101ll,
		     0x222222222ll);
  if (y != 0xfffffffb1b1b1b1bll)
    abort ();

  return 0;
}
