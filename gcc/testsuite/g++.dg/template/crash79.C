// PR c++/36404

struct A
{
  A(int);
  template<int> enum { e }; // { dg-error "template|expected" }
}; // { dg-error "expected" }

A a(A::e);
