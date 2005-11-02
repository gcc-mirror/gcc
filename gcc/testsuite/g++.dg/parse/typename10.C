// PR c++/19253

namespace N { struct X; }

template<typename> struct A
{
  A<typename N::X x> a; // { dg-error "invalid" }
};
