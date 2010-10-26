// PR c++/36404

struct A
{
  A(int);
  template<int> enum { e }; // { dg-error "template|expected" }
};

A a(A::e); // { dg-error "not a member" }
