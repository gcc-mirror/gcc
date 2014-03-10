// PR c++/51327
// { dg-do compile { target c++11 } }

struct A
{
  A(int);
};

struct B : A {};                   // { dg-error "no matching" }

constexpr int foo(B) { return 0; } // { dg-error "invalid type" }
