/* { dg-do run } */
/* { dg-additional-options "-fpredictive-commoning" } */

int a, b, c, d;
void e(int f)
{
  int g[] = {5, 8};
  int *h = g;
  while (c < f) {
    d = 0;
    for (; d < f; d++)
      c = h[d];
  }
}
int main()
{
  int i = (0 == a + 1) - 1;
  e(i + 3);
  if (b != 0)
    __builtin_abort ();
  return 0;
}
