// DR3048 - Empty destructuring expansion statements
// { dg-do compile { target c++11 } }
// { dg-options "" }

struct A {};

void
foo ()
{
  static constexpr A b {};
  template for (constexpr auto a : b)	// { dg-warning "'template for' only available with" "" { target c++23_down } }
    ;
  A c {};
  template for (constexpr auto a : c)	// { dg-warning "'template for' only available with" "" { target c++23_down } }
    ;					// { dg-error "'c' is not a constant expression" "" { target *-*-* } .-1 }
}
