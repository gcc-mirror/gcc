// PR c++/58535

struct A
{
  template<int> virtual void foo(); // { dg-error "templates" }
};
