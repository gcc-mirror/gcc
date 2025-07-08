// C++26 P2686R4 - constexpr structured bindings
// { dg-do compile { target c++11 } }
// { dg-options "" }

struct B {
  int i, j;
  long long k, l;
};

void
foo ()
{
  constexpr B a[3] = { { 1, 2, 3, 4 }, { 5, 6, 7, 8 }, { 9, 10, 11, 12 } };
  constexpr auto [ b, c, d ] = a;			// { dg-warning "structured bindings only available with" "" { target c++14_down } }
							// { dg-warning "structured binding declaration can be 'constexpr' only with" "" { target c++23_down } .-1 }
  constexpr auto [ h, i, j, k ] = a[1];			// { dg-warning "structured bindings only available with" "" { target c++14_down } }
							// { dg-warning "structured binding declaration can be 'constexpr' only with" "" { target c++23_down } .-1 }
  static_assert (b.i == 1 && b.l == 4 && c.j == 6 && c.k == 7 && d.i == 9 && d.k == 11, "");
  static_assert (h == 5 && i == 6 && j == 7 && k == 8, "");
}
