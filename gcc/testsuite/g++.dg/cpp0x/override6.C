// PR c++/120628
// { dg-do compile }
// { dg-options "" }
// { dg-additional-options "-pedantic" { target c++14 } }

namespace U {
  struct A {};
  struct A override = {};
}
namespace V {
  template <int N>
  struct B {};
  template <int N>
  struct B<N> override = {};	// { dg-warning "variable templates only available with" "" { target c++11_down } }
}
struct C {
  struct D {};
  static D foo ();
  struct D override = foo ();	// { dg-warning "non-static data member initializers only available with" "" { target c++98_only } }
};
namespace W {
  struct E { struct F {}; };
  struct E::F override = {};
}
template <int N>
struct V::B<N> override = {};	// { dg-warning "variable templates only available with" "" { target c++11_down } }
