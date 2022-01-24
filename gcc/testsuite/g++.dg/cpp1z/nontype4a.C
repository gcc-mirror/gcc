// PR c++/98930
// { dg-do compile { target c++17 } }

template<int*>
struct A { };

template<class T>
auto impl() {
  static int i;
  return A<&i>();
}

using type = decltype(impl<int>());
using type = decltype(impl<char>()); // { dg-error "conflicting declaration" }
