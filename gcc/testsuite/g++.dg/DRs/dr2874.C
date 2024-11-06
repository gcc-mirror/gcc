// DR 2874 - Qualified declarations of partial specializations
// { dg-do compile { target c++11 } }

namespace N {
  template <typename T>
  struct A;
}

template <>
struct N::A <int>;

template <typename T>
struct N::A <T *>; 
