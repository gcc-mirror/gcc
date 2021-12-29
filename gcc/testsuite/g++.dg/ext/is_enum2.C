// PR c++/99968
// { dg-do compile { target c++11 } }
// { dg-additional-options -g }

template <class T> struct A {
  using type = T;
  static const bool value = false;
};

enum E { e0 = __is_enum(E), e1 = A<E>::value };
