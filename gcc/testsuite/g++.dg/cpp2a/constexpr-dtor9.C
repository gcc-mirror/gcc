// PR c++/97790
// { dg-do compile { target c++20 } }

struct S
{
  int *d;
  int n;
  constexpr S () : d(new int[1]{}), n(1) {}
  constexpr ~S () { delete [] d; }
};

constexpr S
foo ()
{
  return S ();
}

constexpr int
bar ()
{
  return foo ().n;
}

constexpr int
baz ()
{
  return S ().n;
}

constexpr int a = baz ();
constexpr int b = bar ();
