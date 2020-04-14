// { dg-do compile { target c++11 } }

struct empty1 { };
constexpr empty1 foo1() { return {}; }

struct empty2 { };
constexpr empty2 foo2(empty1) { return {}; }

constexpr empty2 a = foo2(foo1());
