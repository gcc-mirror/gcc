// C++26 P1306R5 - Expansion statements
// { dg-do run { target c++23 } }
// { dg-options "" }

#include <span>

constexpr int arr[3] = { 1, 2, 3 };
consteval std::span <const int> foo () { return std::span <const int> (arr); }

int
main ()
{
  int r = 0;
  template for (constexpr auto m : foo ())	// { dg-warning "'template for' only available with" "" { target c++23_down } }
    r += m;
  if (r != 6)
    __builtin_abort ();
}
