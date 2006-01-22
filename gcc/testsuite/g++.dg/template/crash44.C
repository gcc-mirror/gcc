// PR c++/25858

namespace N {
  template<int> struct A {};
}

struct B N::A<0> {}; // { dg-error "invalid" }
