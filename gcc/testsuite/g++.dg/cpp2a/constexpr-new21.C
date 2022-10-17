// PR c++/100495
// { dg-do compile { target c++20 } }

struct S {
  constexpr virtual ~S () {}
};

constexpr bool
foo ()
{
  S *p = new S ();
  delete p;
  return true;
}

constexpr bool x = foo ();
static_assert (x);
