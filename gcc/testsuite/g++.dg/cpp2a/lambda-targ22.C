// PR c++/123665
// { dg-do compile { target c++20 } }

template<class T, class V = decltype([](auto){ return 42; })>
struct A { using type = V; };

template<class T>
auto f() {
  return [](auto) {
    return typename A<T*>::type{}(true);
  }(0);
}

using type = decltype(f<void>());
using type = int;
