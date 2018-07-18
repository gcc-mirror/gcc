// PR c++/79535 - ICE with NSDMI and array
// { dg-do compile { target c++14 } }
// { dg-options -Wno-pedantic }

struct A
{
  int b = 1;
  int c = 2;
  int x[] = { c, 3 }; // { dg-error "initializer for flexible array member" }
};
A a = { 4, 5 };
