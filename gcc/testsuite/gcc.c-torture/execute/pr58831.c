#include <assert.h>

int a, *b, c, d, f, **i, p, q, *r;
short o, j;

static int __attribute__((noinline, noclone))
fn1 (int *p1, int **p2)
{
  int **e = &b;
  for (; p; p++)
    *p1 = 1;
  *e = *p2 = &d;

  assert (r);

  return c;
}

static int ** __attribute__((noinline, noclone))
fn2 (void)
{
  for (f = 0; f != 42; f++)
    {
      int *g[3] = {0, 0, 0};
      for (o = 0; o; o--)
        for (; a > 1;)
          {
            int **h[1] = { &g[2] };
          }
    }
  return &r;
}

int
main (void)
{
  i = fn2 ();
  fn1 (b, i);
  return 0;
}
