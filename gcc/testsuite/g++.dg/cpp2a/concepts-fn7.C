// PR c++/95132
// { dg-do compile { target c++20 } }

template <class T> struct A {
  static auto f() requires false { return T::fail; }
};

template <class T>
concept C = requires { A<T>::f(); };

static_assert(!C<int>);
