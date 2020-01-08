// PR c++/88337 - Implement P1327R1: Allow dynamic_cast/typeid in constexpr.
// { dg-do compile { target c++2a } }
// Here the hint turns out to be wrong: A is a public base of B2, but the
// dynamic_cast operand is not that subobject, but rather a sibling base of
// B2.

struct A { virtual void f(); };
struct B1: A { };
struct B2: A { };
struct C: B1, B2 { };

constexpr C c;
constexpr A *ap = (B1*)&c;
constexpr A &ar = (B1&)c;
constexpr auto p = dynamic_cast<B2*>(ap);
static_assert (p != nullptr);
constexpr auto p2 = dynamic_cast<B2&>(ar);
static_assert(dynamic_cast<B2*>(ap) == (B2*)&c);
static_assert(dynamic_cast<B2*>((B1*)&c) == (B2*)&c);
static_assert(&dynamic_cast<B2&>((B1&)c) == &(B2&)c);
