// PR c++/99790
// { dg-do compile { target c++17 } }

struct A;
struct B { void (*fn) (A *); };
template <typename T>
int foo (const T &);
struct A { int a; static constexpr B b{[] (A *n) { n->*&A::a = 2; }}; };
int a = foo (A::b);
