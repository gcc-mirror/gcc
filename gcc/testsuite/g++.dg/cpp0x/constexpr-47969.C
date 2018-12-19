// PR c++/47969
// { dg-do compile { target c++11 } }

struct A
{
  // constexpr operator int () { return 1; }
};

constexpr A a = A();

int ar[a]; // { dg-error "could not convert" }
// { dg-error "8:size of array .ar. has non-integral" "" { target c++11 } .-1 }
