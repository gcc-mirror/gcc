// P2242R3
// { dg-do compile { target c++14 } }

constexpr int
foo ()
{
  goto lab;	// { dg-error "'goto' in 'constexpr' function only available with" "" { target c++20_down } }
lab:		// { dg-error "'goto' is not a constant expression" "" { target { c++23 } } .-1 }
  return 1;
}
