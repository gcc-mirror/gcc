// PR c++/88690
// { dg-do compile { target c++11 } }

struct A { int a = 1; };
struct B { int b = 0; };
struct C { C() = default; C (const C&) = delete; };
struct D : public B, public C {};
struct E : A { D f; } g{};
