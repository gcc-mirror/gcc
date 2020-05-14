// PR c++/88337 - Implement P1327R1: Allow dynamic_cast/typeid in constexpr.
// { dg-do compile { target c++20 } }

// Protected base.

struct P1 { virtual void p1(); };
struct P2 { virtual void p2(); };
struct B : protected P1 { virtual void b(); };
struct C { virtual void c(); };
struct A : B, C, protected P2 { virtual void a(); };

constexpr A a;

// P1 is a non-public base of A.
constexpr bool b1 = (dynamic_cast<B&>((P1&)a), false); // { dg-error "reference .dynamic_cast. failed" }
// { dg-message "static type .const P1. of its operand is a non-public base class of dynamic type .A." "" { target *-*-* } .-1 }

// Don't error here.
static_assert (dynamic_cast<B*>((P1*)&a) == nullptr);

constexpr bool b2 = (dynamic_cast<C&>((P2&)a), false); // { dg-error "reference .dynamic_cast. failed" }
// { dg-message "static type .const P2. of its operand is a non-public base class of dynamic type .A." "" { target *-*-* } .-1 }

static_assert (dynamic_cast<C*>((P1*)&a) == nullptr);
static_assert (dynamic_cast<C*>((P2*)&a) == nullptr);
