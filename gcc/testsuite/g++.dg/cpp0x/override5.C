// PR c++/120569
// { dg-do compile }
// { dg-options "" }
// { dg-additional-options "-pedantic" { target c++14 } }

namespace U {
  struct A {};
  struct A override {};		// { dg-warning "extended initializer lists only available with" "" { target c++98_only } }
}
namespace V {
  template <int N>
  struct B {};
  template <int N>
  struct B<N> override {};	// { dg-warning "extended initializer lists only available with" "" { target c++98_only } }
}				// { dg-warning "variable templates only available with" "" { target c++11_down } .-1 }
struct C {
  struct D {};
  struct D override {};		// { dg-warning "extended initializer lists only available with" "" { target c++98_only } }
};				// { dg-warning "non-static data member initializers only available with" "" { target c++98_only } .-1 }
namespace W {
  struct E { struct F {}; };
  struct E::F override {};	// { dg-warning "extended initializer lists only available with" "" { target c++98_only } }
}
template <int N>
struct V::B<N> override {};	// { dg-warning "extended initializer lists only available with" "" { target c++98_only } }
				// { dg-warning "variable templates only available with" "" { target c++11_down } .-1 }
