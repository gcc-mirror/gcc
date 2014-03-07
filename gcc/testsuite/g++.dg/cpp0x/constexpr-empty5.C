// { dg-do compile { target c++11 } }

struct A { };
struct B: A { };

constexpr B b { };
constexpr A a = b;
