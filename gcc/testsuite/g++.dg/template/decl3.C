// c++/32560

namespace N {}

template<typename> struct A
{
  int A<typename N::X>; // { dg-error "namespace|argument|before" }
};
