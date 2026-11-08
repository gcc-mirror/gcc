/* { dg-do run } */
/* { dg-options "-O1" } */

__attribute__((noipa)) int
f (long long mm, int a, int b)
{
  int m = (int) mm;
  int y = a + b;
  if ((m & y) == 0 && (y & 4) == 0)
    return 1;
  return 0;
}

__attribute__((noipa)) int
ref (long long mm, int a, int b)
{
  volatile int m = (int) mm;
  volatile int y = a + b;
  volatile int t1 = (m & y) == 0;
  if (!t1) return 0;
  volatile int t2 = (y & 4) == 0;
  if (!t2) return 0;
  return 1;
}

int
main (void)
{
  long long mm = 0x100000000LL;
  int bad = 0;
  for (int a = -16; a <= 16; a++)
    for (int b = -16; b <= 16; b++)
      {
        int got = f (mm, a, b), want = ref (mm, a, b);
        if (got != want)
          bad++;
      }
  if (bad)
    __builtin_abort ();
  return 0;
}
