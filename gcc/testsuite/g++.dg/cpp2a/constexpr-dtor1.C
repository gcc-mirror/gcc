// P0784R7
// { dg-do compile { target c++11 } }

struct S
{
  constexpr S () : s (0) {}
  constexpr ~S () {}	// { dg-error "'constexpr' destructors only available with" "" { target c++17_down } }
  int s;
};
