// PR c++/97895
// { dg-do compile { target c++11 } }

namespace std {
  template<typename T> struct initializer_list {
    const T *ptr;
    decltype(sizeof 0) n;
  };
  auto a = {}; // { dg-error "unable to deduce" }
}
