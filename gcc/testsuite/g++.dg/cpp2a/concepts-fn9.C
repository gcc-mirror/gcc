// PR c++/80637
// { dg-do compile { target c++20 } }

template<class T, class U>
concept same_as = __is_same(T, U);

template<class T>
struct A {
  void f(int) requires same_as<T, int>;
  void f(...) requires (!same_as<T, int>);
};

auto fptr = &A<int>::f;
using type = decltype(fptr);
using type = void (A<int>::*)(int);
