// PR c++/28346

template<int> struct A
{
  int& i;
  A();
  ~A() { &A::i; } // { dg-error "14:cannot create pointer to reference" }
};

A<0> a;
