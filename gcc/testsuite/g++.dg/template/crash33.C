// PR c++/19253

namespace N {}

template<typename> struct A
{
  A<typename N::X<int> > a; // { dg-error "" }
};
