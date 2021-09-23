// PR c++/68942
// { dg-do compile { target c++11 } }
// A template-id analogue of koenig12.C.

template <int> void foo(...) = delete;

template <class T> void lookup(T t) { foo<0>(t); }

namespace N {
 struct A { };
 template <int> int foo(A);
}

int main() {
  lookup(N::A{});
}
