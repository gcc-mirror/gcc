/* This testcase failed, because scope containing baz was deleted
   (spanned 0 basic blocks) and DWARF-2 couldn't find baz origin.  */
/* { dg-do compile } */

extern void abort (void);

struct A { char *a, *b, *c, *d; };

static int
bar (struct A *x)
{
  return x->c - x->b;
}

static int
bar2 (struct A *x)
{
  int a = x->c - x->b;
  x->c += 26;
  return a;
}

void fnptr (void (*fn) (void));

void
foo (void)
{
  struct A e;

  if (bar2 (&e) < 0)
    abort ();
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
