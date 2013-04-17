/* { dg-do run } */
/* { dg-options "-O2 --save-temps -fno-inline" } */

extern void abort (void);
typedef long long s64;

int
adds_ext (s64 a, int b, int c)
{
 s64 d = a + b;

  if (d == 0)
    return a + c;
  else
    return b + d + c;
}

int
adds_shift_ext (s64 a, int b, int c)
{
 s64 d = (a + ((s64)b << 3));

  if (d == 0)
    return a + c;
  else
    return b + d + c;
}

int main ()
{
  int x;
  s64 y;

  x = adds_ext (0x13000002ll, 41, 15);
  if (x != 318767203)
    abort ();

  x = adds_ext (0x50505050ll, 29, 4);
  if (x != 1347440782)
    abort ();

  x = adds_ext (0x12121212121ll, 2, 14);
  if (x != 555819315)
    abort ();

  x = adds_shift_ext (0x123456789ll, 4, 12);
  if (x != 591751097)
    abort ();

  x = adds_shift_ext (0x02020202ll, 9, 8);
  if (x != 33686107)
    abort ();

  x = adds_shift_ext (0x987987987987ll, 23, 41);
  if (x != -2020050305)
    abort ();

  return 0;
}

/* { dg-final { scan-assembler-times "adds\tx\[0-9\]+, x\[0-9\]+, x\[0-9\]+, sxtw" 2 } } */
