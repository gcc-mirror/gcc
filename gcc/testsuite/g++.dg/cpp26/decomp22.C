// C++26 P2686R4 - constexpr structured bindings
// { dg-do compile { target c++11 } }
// { dg-options "" }

namespace std {
  template <typename T> struct tuple_size;
  template <int, typename> struct tuple_element;
}

struct A {
  int i, j;
  template <int I> constexpr const int &get () const { return I == 1 ? j : i; }
};

template <> struct std::tuple_size <const A> { static const int value = 3; };
template <int I> struct std::tuple_element <I, const A> { using type = const int; };

constexpr struct B {
  int i, j;
  long long k, l;
} a[3] = { { 1, 2, 3, 4 }, { 5, 6, 7, 8 }, { 9, 10, 11, 12 } };

constexpr auto [ b, c, d ] = a;				// { dg-warning "structured bindings only available with" "" { target c++14_down } }
							// { dg-warning "structured binding declaration can be 'constexpr' only with" "" { target c++23_down } .-1 }
#if __cpp_constinit >= 201907
constinit auto [ e, f, g ] = a;				// { dg-warning "'constinit' can be applied to structured binding only with" "" { target { c++20 && c++23_down } } }
#endif
constexpr auto [ h, i, j, k ] = a[1];			// { dg-warning "structured bindings only available with" "" { target c++14_down } }
							// { dg-warning "structured binding declaration can be 'constexpr' only with" "" { target c++23_down } .-1 }
#if __cpp_constinit >= 201907
constinit auto [ l, m, n, o ] = a[2];			// { dg-warning "'constinit' can be applied to structured binding only with" "" { target { c++20 && c++23_down } } }
#endif
constexpr auto & [ p, q, r ] = a;			// { dg-warning "structured bindings only available with" "" { target c++14_down } }
							// { dg-warning "structured binding declaration can be 'constexpr' only with" "" { target c++23_down } .-1 }
#if __cpp_constinit >= 201907
constinit auto & [ s, t, u ] = a;			// { dg-warning "'constinit' can be applied to structured binding only with" "" { target { c++20 && c++23_down } } }
#endif
constexpr auto & [ v, w, x, y ] = a[1];			// { dg-warning "structured bindings only available with" "" { target c++14_down } }
							// { dg-warning "structured binding declaration can be 'constexpr' only with" "" { target c++23_down } .-1 }
#if __cpp_constinit >= 201907
constinit auto & [ aa, ab, ac, ad ] = a[2];		// { dg-warning "'constinit' can be applied to structured binding only with" "" { target { c++20 && c++23_down } } }
#endif
static_assert (b.i == 1 && b.l == 4 && c.j == 6 && c.k == 7 && d.i == 9 && d.k == 11, "");
static_assert (h == 5 && i == 6 && j == 7 && k == 8, "");
static_assert (p.i == 1 && p.l == 4 && q.j == 6 && q.k == 7 && r.i == 9 && r.k == 11, "");
static_assert (&p.i == &a[0].i && &p.l == &a[0].l && &q.j == &a[1].j, "");
static_assert (&q.k == &a[1].k && &r.i == &a[2].i && &r.k == &a[2].k, "");
static_assert (v == 5 && w == 6 && x == 7 && y == 8, "");
static_assert (&v == &a[1].i && &w == &a[1].j && &x == &a[1].k && &y == &a[1].l, "");

constexpr A z = { 42, -42 };
constexpr auto [ ae, af, ag ] = z;			// { dg-warning "structured bindings only available with" "" { target c++14_down } }
							// { dg-warning "structured binding declaration can be 'constexpr' only with" "" { target c++23_down } .-1 }
#if __cpp_constinit >= 201907
constinit const auto [ ah, ai, aj ] = z;		// { dg-warning "'constinit' can be applied to structured binding only with" "" { target { c++20 && c++23_down } } }
#endif
constexpr auto & [ ak, al, am ] = z;			// { dg-warning "structured bindings only available with" "" { target c++14_down } }
							// { dg-warning "structured binding declaration can be 'constexpr' only with" "" { target c++23_down } .-1 }
#if __cpp_constinit >= 201907
constinit auto & [ an, ao, ap ] = z;			// { dg-warning "'constinit' can be applied to structured binding only with" "" { target { c++20 && c++23_down } } }
#endif
static_assert (ae == 42 && af == -42 && ag == 42, "");
static_assert (&af == &ae + 1 && &ag == &ae, "");
static_assert (&ae != &z.i && &af != &z.j && &ag != &z.i, "");
static_assert (ak == 42 && al == -42 && am == 42, "");
static_assert (&ak == &z.i && &al == &z.j && &am == &z.i, "");
