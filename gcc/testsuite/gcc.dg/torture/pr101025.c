/* { dg-do run } */

int a[10];
int b, d, g;
volatile char c;
short e;
volatile int f;
int main()
{
  for (; d <= 9; d++) {
      b = e = 0;
      for (; e < 4; e++)
        a[e] = 4;
      for (; b <= 3; b++)
        if (g)
          f = 0;
        else
          a[b] = c;
  }
  if (a[1] != 0)
    __builtin_abort ();
  return 0;
}
