// PR ipa/68672
// { dg-do compile }
// { dg-options "-O3 --param=partial-inlining-entry-probability=100 -g" }

struct S { ~S () {} };
S *a, *e;
int *b;
void bar ();
void baz ();
void fn (int *);
void fn2 (S *);

static int
foo ()
{
  S *c = a;
  if (c)
    {
      bar ();
      if (a)
	__builtin_abort ();
      baz ();
    }
  int p = *b;
  S *f = e;
  if (p)
    {
      fn2 (f);
      fn (b); fn (b); fn (b); fn (b); fn (b); fn (b); fn (b); fn (b); fn (b); fn (b);
      fn (b); fn (b); fn (b); fn (b); fn (b); fn (b); fn (b); fn (b); fn (b); fn (b);
      fn (b); fn (b); fn (b); fn (b); fn (b); fn (b); fn (b); fn (b); fn (b); fn (b);
      fn (b); fn (b); fn (b); fn (b); fn (b); fn (b); fn (b); fn (b); fn (b); fn (b);
    }
  f->~S ();
  int q = 2 * p;
  int r = 3 * q;
  S *d = c;
  return p;
}

void
use1 ()
{
  foo ();
}

void
use2 ()
{
  foo ();
}

void
use3 ()
{
  foo ();
}
