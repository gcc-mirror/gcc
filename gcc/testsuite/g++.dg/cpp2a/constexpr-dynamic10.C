// PR c++/88337 - Implement P1327R1: Allow dynamic_cast/typeid in constexpr.
// { dg-do compile { target c++20 } }

// Virtual base.

struct C { virtual void a(); };
struct B { virtual void b(); };
struct A : virtual B, C { virtual void c(); }; // { dg-error ".struct A. has virtual base classes" }

constexpr A a; // { dg-error "call" }

constexpr bool b1 = (dynamic_cast<C&>((B&)a), false);
