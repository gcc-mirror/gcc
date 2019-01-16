// PR c++/77549
// { dg-do compile }

struct A
{ 
  static int x;
};

void
f1 ()
{ 
  using ::A;
  x;			// { dg-error "'x' was not declared in this scope" }
}

namespace N
{
  int bar;
}

void
f2 ()
{
  using N::bar;
  baz++;		// { dg-error "'baz' was not declared in this scope; did you mean 'bar'\\?" }
}

int
bar ()
{
  return 0;
}

namespace M
{
  int
  bar ()
  {
    return 0;
  }
}

void
f3 ()
{
  using M::bar;
  baz ();		// { dg-error "'baz' was not declared in this scope; did you mean 'bar'\\?" }
}

namespace O
{
  int
  foo ()
  {
    return 0;
  }
}

namespace P
{
  int
  bar ()
  {
    return 0;
  }
}

void
f4 ()
{
  using O::foo;
  using P::bar;
  fooo ();		// { dg-error "'fooo' was not declared in this scope; did you mean 'foo'\\?" }
  baz ();		// { dg-error "'baz' was not declared in this scope; did you mean 'bar'\\?" }
}
