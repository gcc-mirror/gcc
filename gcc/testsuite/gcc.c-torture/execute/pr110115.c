/* PR middle-end/110115 */

int a;
signed char b;

static int
foo (signed char *e, int f)
{
  int d;
  for (d = 0; d < f; d++)
    e[d] = 0;
  return d;
}

int
bar (signed char e, int f)
{
  signed char h[20];
  int i = foo (h, f);
  return i;
}

int
baz ()
{
  switch (a)
    {
    case 'f':
      return 0;
    default:
      return ~0;
    }
}

int
main ()
{
  {
    signed char *k[3];
    int d;
    for (d = 0; bar (8, 15) - 15 + d < 1; d++)
      k[baz () + 1] = &b;
    *k[0] = -*k[0];
  }
}
