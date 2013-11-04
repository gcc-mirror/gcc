// PR c++/51327
// { dg-options -std=c++11 }

struct A
{
  A(int);
};

struct B : A {};                   // { dg-error "no matching" }

constexpr int foo(B) { return 0; } // { dg-error "invalid type" }
