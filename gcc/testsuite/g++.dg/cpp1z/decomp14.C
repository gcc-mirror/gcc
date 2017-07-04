// { dg-do compile { target c++11 } }
// { dg-options "" }

struct A { int f; };
struct B { int b; };
struct C : virtual A {};
struct D : virtual A {};
struct E { int f; };
struct F : A { int f; };
struct G : A, E {};
struct H : C, D {};
struct I : A, C {};		// { dg-warning "due to ambiguity" }
struct J : B {};
struct K : B, virtual J {};	// { dg-warning "due to ambiguity" }
struct L : virtual J {};
struct M : virtual J, L {};

void
foo (C &c, F &f, G &g, H &h, I &i, K &k, M &m)
{
  auto [ ci ] = c;		// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  auto [ fi ] = f;		// { dg-error "cannot decompose class type 'F': both it and its base class 'A' have non-static data members" }
				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  auto [ gi ] = g;		// { dg-error "cannot decompose class type 'G': its base classes 'A' and 'E' have non-static data members" }
				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  auto [ hi ] = h;		// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  auto [ ki ] = k;		// { dg-error "'B' is an ambiguous base of 'K'" }
				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  auto [ mi ] = m;		// { dg-warning "structured bindings only available with" "" { target c++14_down } }
}
