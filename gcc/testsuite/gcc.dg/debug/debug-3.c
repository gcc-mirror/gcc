/* This testcase failed, because scope containing baz was deleted
   (spanned 0 basic blocks) and DWARF-2 couldn't find baz origin.  */
/* { dg-do compile } */

struct A { char *a, *b, *c, *d; };

static int
bar (struct A *x)
{
  return x->c - x->b;
}

void fnptr (void (*fn) (void));

void
foo (void)
{
  struct A e;

  {
    void baz (void)
      {
	bar (&e);
      }
    fnptr (baz);
  }
  {
    struct A *f;

    f = &e;
    if (f->c - f->a > f->d - f->a)
      f->c = f->d;
  }
}
