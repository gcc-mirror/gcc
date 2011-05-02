// PR c++/47969
// { dg-options -std=c++0x }

struct A
{
  // constexpr operator int () { return 1; }
};

constexpr A a = A();

int ar[a]; // { dg-error "has non-integral type" }
