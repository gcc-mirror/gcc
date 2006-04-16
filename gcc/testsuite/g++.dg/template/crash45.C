// PR c++/26365

struct A {};

namespace N
{
  template<int> void foo();
}

void bar(A *p)
{
  p->N::foo<0>; // { dg-error "not a member" } 
}
