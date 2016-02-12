// PR ipa/68672
// { dg-do compile }
// { dg-options "-O3 --param=partial-inlining-entry-probability=100 -g" }

struct S { ~S () {} };
S *a;
int *b;
void bar ();
void baz ();
void fn (int *);

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
  if (p)
    {
      fn (b); fn (b); fn (b); fn (b); fn (b); fn (b); fn (b); fn (b); fn (b); fn (b);
      fn (b); fn (b); fn (b); fn (b); fn (b); fn (b); fn (b); fn (b); fn (b); fn (b);
      fn (b); fn (b); fn (b); fn (b); fn (b); fn (b); fn (b); fn (b); fn (b); fn (b);
      fn (b); fn (b); fn (b); fn (b); fn (b); fn (b); fn (b); fn (b); fn (b); fn (b);
    }
  c->~S ();
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
