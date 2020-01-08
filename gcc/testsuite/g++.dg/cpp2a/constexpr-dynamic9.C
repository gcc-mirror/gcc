// PR c++/88337 - Implement P1327R1: Allow dynamic_cast/typeid in constexpr.
// { dg-do compile { target c++2a } }

// Ambiguous base.

struct A { virtual void a(); };
struct B : A { virtual void b(); };
struct C : A { virtual void c(); };
struct D { virtual void a(); };
struct E : B, C, D { virtual void d(); };

constexpr E e;

constexpr bool b1 = (dynamic_cast<A&>((D&)e), false); // { dg-error "reference .dynamic_cast. failed" }
// { dg-message ".A. is an ambiguous base class of dynamic type .E. of its operand" "" { target *-*-* } .-1 }

static_assert (dynamic_cast<A*>((D*)&e) == nullptr);
