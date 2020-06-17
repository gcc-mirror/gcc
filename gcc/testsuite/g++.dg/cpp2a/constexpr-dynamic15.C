// PR c++/88337 - Implement P1327R1: Allow dynamic_cast/typeid in constexpr.
// { dg-do compile { target c++20 } }
// Test HINT = -3 (SRC is a multiple public non-virtual base of DST).

struct A { virtual void a() {} };
struct C : A { };
struct D : A { };
struct B : C, D { };

constexpr B b;
static_assert (&dynamic_cast<B&>((A&)(C&)b) == &b);
static_assert (&dynamic_cast<B&>((A&)(D&)b) == &b);
static_assert (dynamic_cast<B*>((A*)(C*)&b) == &b);
static_assert (dynamic_cast<B*>((A*)(D*)&b) == &b);
