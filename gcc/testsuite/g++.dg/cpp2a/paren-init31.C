// PR c++/92812
// P1975R0
// { dg-do compile { target c++20 } }
// Test we don't lifetime-extend the int temporary.

struct A { const int &r; };
A a(42);
auto a2 = static_cast<A>(42);

// { dg-final { scan-assembler-not "_ZGR" } }
