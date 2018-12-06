// PR c++/87506
// { dg-do compile { target c++11 } }

struct A {};
struct B { constexpr B (const A) {} };
struct C : B { using B::B; };

void
foo ()
{
  C c (A{});
}
