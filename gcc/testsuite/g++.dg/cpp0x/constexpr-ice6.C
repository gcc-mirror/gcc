// PR c++/51327
// { dg-do compile { target c++11 } }

struct A
{
  A(int);
};

struct B : A {};                   // { dg-message "" "" { target c++20_down } }

constexpr int foo(B) { return 0; } // { dg-error "invalid type" "" { target c++20_down } }
