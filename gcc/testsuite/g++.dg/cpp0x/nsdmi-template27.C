// PR c++/126811
// { dg-do compile { target c++11 } }

struct M { constexpr M () : x (0) {} int x; };

struct A { double m = 1; };			// FLOAT_EXPR
struct B { int m = 1.0; };			// FIX_TRUNC_EXPR
struct C { __complex__ double c = 1; };		// COMPLEX_EXPR
struct D { double a = 1; double m = a; };	// PLACEHOLDER_EXPR
struct E { M m = M (); };			// TARGET_EXPR

// The initializers must still be constant-evaluated.
template<class T> void g ()
{
  constexpr A a = {};
  static_assert (a.m == 1.0, "");
  constexpr B b = {};
  static_assert (b.m == 1, "");
  constexpr C c = {};
  static_assert (__real__ c.c == 1.0, "");
  constexpr D d = {};
  static_assert (d.m == 1.0, "");
  constexpr E e = {};
  static_assert (e.m.x == 0, "");
}
