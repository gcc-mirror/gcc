// PR c++/58607
// { dg-do compile { target c++11 } }

struct A
{
  constexpr A() { i; }  // { dg-error "declared|empty body" }
};
