// PR c++/81197
// { dg-do compile { target c++11 } }
// { dg-options "" }

struct X { int a; };
struct Y { int b, c, d; };
auto&& [t] = X{};	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
namespace A { namespace B { auto&& [u, v, ww] = Y{}; } }	// { dg-warning "structured bindings only available with" "" { target c++14_down } }

// { dg-final { scan-assembler "_ZGRDC1tE_" } }
// { dg-final { scan-assembler "_ZGRN1A1BDC1u1v2wwEE_" } }
