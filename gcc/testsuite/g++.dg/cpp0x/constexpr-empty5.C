// { dg-options -std=c++11 }

struct A { };
struct B: A { };

constexpr B b { };
constexpr A a = b;
