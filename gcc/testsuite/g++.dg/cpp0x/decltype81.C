// PR c++/105386
// { dg-do compile { target c++11 } }

template<class T> struct NoInst {
  static_assert(sizeof(T) == 9999, "NoInst instantiated");
};

template<class T> NoInst<T> f(T);

template<class>
struct A {
  using type = decltype(f(0));
};

A<int> a;
