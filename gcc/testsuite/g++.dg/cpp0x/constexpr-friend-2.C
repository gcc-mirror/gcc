// { dg-do compile { target c++11 } }

template<typename T> void f(T);

template <class T> class A {
  friend constexpr void f<>(int);
};
