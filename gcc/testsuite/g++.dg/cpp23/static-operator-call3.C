// P1169R4 - static operator()
// { dg-do compile { target c++14 } }
// { dg-options "" }

void
foo ()
{
  auto a = [] (auto x) static { return x; };					// { dg-warning "'static' only valid in lambda with" "" { target c++20_down } }
  int (*b) (int) = a;
}
