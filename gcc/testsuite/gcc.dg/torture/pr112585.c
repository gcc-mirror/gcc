/* { dg-do run } */
/* { dg-additional-options "-ftree-loop-distribution" } */

static int a[10], b;
char c, *d = &c, e;
int main()
{
  int f = 0;
  for (; f < 9; f++) {
    a[f] = 1;
    a[f + 1] = e;
    for (b = 0; b < 8; b++)
      *d = 0;
  }
  if (a[2] != 1)
    __builtin_abort();
  return 0;
}
