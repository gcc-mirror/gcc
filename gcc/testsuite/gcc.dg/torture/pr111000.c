/* { dg-do run } */

volatile int a = 68;
int b, d, e;
int main()
{
  int t = a;
  for (; d <= 6; d++) {
    for (b = 0; b <= 6; b++) {
      if (t >= 31)
        e = d;
      else if (d > (647 >> t))
        e = d;
      else
        e = 0;
    }
  }
  if (e != 6)
    __builtin_abort();
  return 0;
}
