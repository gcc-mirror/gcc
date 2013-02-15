// PR c++/55003
// { dg-do compile { target c++11 } }

template<typename T>
struct A {
  static const auto t
    = (typename T::type)42;
};

struct X {
  typedef int type;
};

A<X> a;
