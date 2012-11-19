// Origin: PR c++/54466
// { dg-do compile { target c++11 } }

template<typename T>
  struct X { };

template<typename T>
  using Y = const X<T>;

using Z = Y<int>;
