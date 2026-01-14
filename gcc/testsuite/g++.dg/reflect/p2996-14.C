// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test from [expr.reflect]/5.

using info = decltype(^^void);

struct A { struct S {}; };
struct B : A { using A::S; };
constexpr info r1 = ^^B::S;	// { dg-error "cannot be applied to a using-declaration" }
struct C : virtual B { struct S {}; };
struct D : virtual B, C {};
D::S s;
constexpr info r2 = ^^D::S;// OK, names C::S per 6.5.2
// OK, result C::S not found through using-declarator

struct E : A { };
constexpr info r3 = ^^E::S;
