// PR c++/120716
// { dg-do compile { target c++11 } }

struct A { int *r; };

void
foo ()
{
  static int i;
  constexpr A a = { &i };
  static_assert (a.r == &i, "");
  [&] { static_assert (a.r != nullptr, ""); } ();
}
