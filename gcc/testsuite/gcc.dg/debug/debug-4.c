/* This testcase failed, because scope containing baz was not emitted
   (doesn't contain any instructions) and DWARF-2 couldn't find baz origin.  */
/* { dg-do compile } */

struct A { char *a, *b, *c, *d; };

static int
bar (struct A *x)
{
  return x->c - x->b;
}

void
foo (void)
{
  struct A e;

  {
    int baz (void)
      {
	return bar (&e);
      }
  }
  if (e.c - e.a > e.d - e.a)
    e.c = e.d;
}
