// PR c++/25638

struct A { ~A(); };

struct B : A
{
  template<int> friend A::~A(); // { dg-error "member template" }
};
