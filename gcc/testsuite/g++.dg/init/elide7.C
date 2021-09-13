// PR c++/98744
// { dg-additional-options "-O2 -fno-inline -Wmaybe-uninitialized" }

struct A {};
struct B : virtual A {};
struct C : B {
  C() : B(B()) {}
};
int main() { C c; return 0; }
