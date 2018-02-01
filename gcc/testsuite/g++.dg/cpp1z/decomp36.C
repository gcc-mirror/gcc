// PR c++/84031
// { dg-do compile { target c++11 } }
// { dg-options "" }

struct A { unsigned char : 1, a1 : 1, a2 : 2, : 1, a3 : 3; };
struct B { unsigned char : 1, : 7; };
struct C : B { constexpr C () : c1 (1), c2 (2), c3 (3) {} unsigned char : 1, c1 : 1, c2 : 2, : 1, c3 : 3; };
struct D : C { constexpr D () {} unsigned char : 1, : 7; };

int
main ()
{
  static constexpr A a { 1, 2, 3 };
  const auto &[a1, a2, a3] = a;		// { dg-warning "only available with" "" { target c++14_down } }
  static_assert (a1 == 1 && a2 == 2 && a3 == 3, "");
  static constexpr D d;
  const auto &[d1, d2, d3] = d;		// { dg-warning "only available with" "" { target c++14_down } }
  static_assert (d1 == 1 && d2 == 2 && d3 == 3, "");
}
