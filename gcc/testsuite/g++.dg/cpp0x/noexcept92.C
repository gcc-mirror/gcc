// PR c++/123189
// { dg-do compile { target c++11 } }

template<class T> void f() noexcept(noexcept(T()));

template<class T>
struct A {
  friend void f<T>() noexcept(noexcept(T()));
};

template struct A<int>;
