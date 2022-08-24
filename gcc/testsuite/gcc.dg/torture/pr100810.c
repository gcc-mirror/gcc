/* { dg-do run } */

int a, b = 1, c = 1, e, f = 1, g, h, j;
volatile int d;
static void k()
{
  int i;
  h = b;
  if (c && a >= 0) {
      while (a) {
	  i++;
	  h--;
      }
      if (g)
	for (h = 0; h < 2; h++)
	  ;
      if (!b)
	i &&d;
  }
}
static void l()
{
  for (; j < 1; j++)
    if (!e && c && f)
      k();
}
int main()
{
  if (f)
    l();
  if (h != 1)
    __builtin_abort();
  return 0;
}
