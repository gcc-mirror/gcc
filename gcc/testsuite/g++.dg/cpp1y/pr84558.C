// PR c++/84558
// { dg-do compile { target c++14 } }

struct A { static int i; constexpr A () { i = 0; } };
struct B { A a[2][3][4]; };
B b;
