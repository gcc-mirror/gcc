// PR c++/55993
// { dg-do compile { target c++11 } }

struct A {};
struct B:A {};
struct C:A {};
struct D:B,C {};

constexpr D d {};
constexpr const C& e=d; // OK
constexpr auto f=static_cast<const C&>(d); // FAIL
