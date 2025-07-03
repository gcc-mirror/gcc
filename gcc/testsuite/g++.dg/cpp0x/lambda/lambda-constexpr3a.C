// PR c++/120716
// { dg-do compile { target c++11 } }

struct A { int *const &r; };

void
foo (int x)
{
  constexpr A a = { &x };	// { dg-error "constant" }
  static_assert (a.r == &x, "");	// { dg-error "non-constant" }
  [&] { static_assert (a.r != nullptr, ""); } (); // { dg-error "non-constant" }
}
