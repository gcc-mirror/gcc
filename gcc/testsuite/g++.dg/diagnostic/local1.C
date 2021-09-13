// PR c++/99213
// { dg-do compile { target c++14 } }

template <class T>
struct A {
  template <class U>
  static auto f() {
    struct S1{};
    return S1{};
  }
};

using type = void;
using type = decltype(A<int>::f<char>()); // { dg-error "A<int>::f<char>\\(\\)::S1"  }

template <class T>
struct B {
  static auto f() {
    struct S2{};
    return S2{};
  }
};

using type = void;
using type = decltype(B<int>::f()); // { dg-error "B<int>::f\\(\\)::S2"  }
