// PR c++/25666

struct A { ~A(); };

struct B
{
  template<int> friend A::~A();  // { dg-error "member template" }
};
