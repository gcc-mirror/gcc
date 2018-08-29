/* { dg-do run } */

int a[2] = { 0, 1 };
int x = 129;

int
main ()
{
  volatile int v = 0;
  int t = x, i;
  for (i = 0; i < 1 + v + v + v + v + v + v + v + v + a[a[0]]; i++)
    t = a[(signed char) (130 - x)];
  if (t != 1)
    __builtin_abort ();
  return 0;
}
