/* { dg-do run } */
/* { dg-options "-O2 --save-temps -fno-inline" } */

extern void abort (void);
typedef long long s64;

int
subs_ext (s64 a, int b, int c)
{
 s64 d = a - b;

  if (d == 0)
    return a + c;
  else
    return b + d + c;
}

int
subs_shift_ext (s64 a, int b, int c)
{
 s64 d = (a - ((s64)b << 3));

  if (d == 0)
    return a + c;
  else
    return b + d + c;
}

int main ()
{
  int x;
  s64 y;

  x = subs_ext (0x13000002ll, 41, 15);
  if (x != 318767121)
    abort ();

  x = subs_ext (0x50505050ll, 29, 4);
  if (x != 1347440724)
    abort ();

  x = subs_ext (0x12121212121ll, 2, 14);
  if (x != 555819311)
    abort ();

  x = subs_shift_ext (0x123456789ll, 4, 12);
  if (x != 591751033)
    abort ();

  x = subs_shift_ext (0x02020202ll, 9, 8);
  if (x != 33685963)
    abort ();

  x = subs_shift_ext (0x987987987987ll, 23, 41);
  if (x != -2020050673)
    abort ();

  return 0;
}

/* { dg-final { scan-assembler-times "subs\tx\[0-9\]+, x\[0-9\]+, x\[0-9\]+, sxtw" 2 } } */
