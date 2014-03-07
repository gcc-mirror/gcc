// PR c++/46369
// { dg-do compile { target c++11 } }

struct A
{
  unsigned i : 1;
};

constexpr A f() { return { 1 }; }
constexpr bool b = (f().i == 1);
