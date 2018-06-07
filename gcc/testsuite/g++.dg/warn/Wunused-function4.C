// PR c++/80598
// { dg-do compile }
// { dg-options "-Wunused-function" }

static void
foo ()		// { dg-bogus "defined but not used" }
{
}

static void
bar ()		// { dg-warning "defined but not used" }
{
}

template <class T>
int
baz (T x)
{
  foo ();
  return 0;
}
