// { dg-options -std=c++0x }

struct A { };
struct B: A { };

constexpr B b { };
constexpr A a = b;
