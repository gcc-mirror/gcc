// PR c++/37649

struct A
{
  template<int> struct {}; // { dg-error "template class without a name" }
};
