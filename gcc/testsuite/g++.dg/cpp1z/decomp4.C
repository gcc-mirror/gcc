// { dg-do compile { target c++11 } }
// { dg-options "" }

struct A { int a; struct { int b; }; };
struct B { int a; union { int c; long d; }; };
struct C { int a; private: int b; };
struct D { int a; private: static int b; };
struct E { protected: int a; };
struct F { int a; };
struct G : public F { int b; };
struct H { int b; };
struct I : public F, H {};

void
test (A &a, B &b, C &c, D &d, E &e, F &f, G &g, H &h, I &i)
{
  auto [ j ] = a;			// { dg-error "cannot decompose class type 'A' because it has an anonymous struct member" }
					// { dg-warning "decomposition declaration only available with -std=c..1z or -std=gnu..1z" "" { target c++14_down } .-1 }
  auto [ k ] { b };			// { dg-error "cannot decompose class type 'B' because it has an anonymous union member" }
					// { dg-warning "decomposition declaration only available with -std=c..1z or -std=gnu..1z" "" { target c++14_down } .-1 }
  auto [ l, l2 ] = c;			// { dg-error "cannot decompose non-public member 'C::b' of 'C'" }
					// { dg-warning "decomposition declaration only available with -std=c..1z or -std=gnu..1z" "" { target c++14_down } .-1 }
  auto [ m ] = d;			// { dg-warning "decomposition declaration only available with -std=c..1z or -std=gnu..1z" "" { target c++14_down } }
  auto [ n ] { e };			// { dg-error "cannot decompose non-public member 'E::a' of 'E'" }
					// { dg-warning "decomposition declaration only available with -std=c..1z or -std=gnu..1z" "" { target c++14_down } .-1 }
  auto [ o ] { f };			// { dg-warning "decomposition declaration only available with -std=c..1z or -std=gnu..1z" "" { target c++14_down } }
  auto & [ p ] { g };			// { dg-error "cannot decompose class type 'G': both it and its base class 'F' have non-static data members" }
					// { dg-warning "decomposition declaration only available with -std=c..1z or -std=gnu..1z" "" { target c++14_down } .-1 }
  auto [ q ] { h };			// { dg-warning "decomposition declaration only available with -std=c..1z or -std=gnu..1z" "" { target c++14_down } }
  auto [ r ] { i };			// { dg-error "cannot decompose class type 'I': its base classes 'F' and 'H' have non-static data members" }
					// { dg-warning "decomposition declaration only available with -std=c..1z or -std=gnu..1z" "" { target c++14_down } .-1 }
}
