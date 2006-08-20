// PR c++/28346

template<int> struct A
{
  int& i;
  A();
  ~A() { &A::i; } // { dg-error "reference" }
};

A<0> a; // { dg-error "instantiated" }
