// PR c++/99031
// { dg-do compile { target c++20 } }

constexpr bool
foo ()
{
  auto a = new int;
  auto b = new int;
  bool r = a == b;
  delete b;
  delete a;
  return r;
}

static_assert (!foo ());
