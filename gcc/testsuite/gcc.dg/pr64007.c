/* { dg-options " -O3 " } */
/* { dg-do run } */

#include <assert.h>

int d, i;

struct S
{
  int f0;
} *b, c, e, h, **g = &b;

static struct S *f = &e;

int
fn1 (int p)
{
  int a = 0;
  return a || p < 0 || p >= 2 || 1 >> p;
}

int
main ()
{
  int k = 1, l, *m = &c.f0;

  for (;;)
    {
      l = fn1 (i);
      *m = k && i;
      if (l)
	{
	  int n[1] = {0};
	}
      break;
    }

  *g = &h;

  assert (b);

  if (d)
    (*m)--;
  d = (f != 0) | (i >= 0);

  if (c.f0 != 0)
    __builtin_abort ();

  return 0;
}
