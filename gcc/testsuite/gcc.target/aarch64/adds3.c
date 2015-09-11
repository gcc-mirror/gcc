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
    return d;
}

int
adds_shift_ext (s64 a, int b, int c)
{
 s64 d = (a + ((s64)b << 3));

  if (d == 0)
    return a + c;
  else
    return d;
}

int main ()
{
  int x;
  s64 y;

  x = adds_ext (0x13000002ll, 41, 15);
  if (x != (int)(0x13000002ll + 41))
    abort ();

  x = adds_ext (0x50505050ll, -0x50505050ll, 4);
  if (x != (int)(0x50505050ll + 4))
    abort ();

  x = adds_ext (0x12121212121ll, 2, 14);
  if (x != (int)(0x12121212121ll + 2))
    abort ();

  x = adds_shift_ext (0x123456789ll, 4, 12);
  if (x != (int)(0x123456789ll + (4 << 3)))
    abort ();

  x = adds_shift_ext (-(0x02020202ll << 3), 0x02020202ll, 8);
  if (x != (int)(8 - (0x02020202ll << 3)))
    abort ();

  x = adds_shift_ext (0x987987987987ll, 23, 41);
  if (x != (int)(0x987987987987ll + (23 << 3)))
    abort ();

  return 0;
}

/* { dg-final { scan-assembler-times "adds\tx\[0-9\]+, x\[0-9\]+, x\[0-9\]+, sxtw" 2 } } */
