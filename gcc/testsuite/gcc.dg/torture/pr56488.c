/* { dg-do run { target { size32plus } } } */

int a, c, d = 1;
struct S { int s; } b, f;
short e;

static void
foo (int x)
{
  int g[] = { };
  for (e = 0; e != 1; e = e + 5)
    {
      int *h[1] = { &g[0] };
      if (!x)
	return;
      f = b;
    }
}

int
main ()
{
  int i, j;
  for (i = 0; i < 6; i++)
    for (j = 8; j; j--)
      a = 0;
  foo (d);
  while (c)
    ;
  return 0;
}
