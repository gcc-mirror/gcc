// PR target/70245
// { dg-do run }
// { dg-additional-sources "pr70245-aux.cc" }
// { dg-options "-O2" }
// { dg-additional-options "-fPIC" { target fpic } }
// { dg-additional-options "-march=i386 -mtune=atom" { target ia32 } }

#include "pr70245.h"

struct A *a, *i;
int b, c, e, l;
F d;

static A *
foo (B *x, int *y, int *z)
{
  unsigned char *f = (unsigned char *) fn3 (y);
  D *g = (D *) f;
  A *h;
  if (e || a || c || b || g->d)
    return 0;
  h = (A *) fn4 ();
  __builtin_memcpy (h, a, sizeof (A));
  h->a1 = *(D *) f;
  if (d)
    {
      d (h, x, f + g->d, z);
      if (*z)
	fn2 ();
    }
  return h;
}

static A *
bar (B *x, int *y)
{
  int *j = fn1 (x->b, y);
  if (*y > 0)
    return 0;
  i = foo (x, j, y);
  return i;
}

B k;

void
baz (int x)
{
  if (x)
    bar (0, 0);
  bar (&k, &l);
}
