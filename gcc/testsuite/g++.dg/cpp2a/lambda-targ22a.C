// PR c++/123665
// { dg-do compile { target c++20 } }
// A more elaborate version of lambda-targ22.C.

template<class T, class U, class V>
struct W { };

template<class T, class U,
	 class V = decltype([](auto x){ return W<T, U, decltype(x)>{}; })>
struct A { using type = V; };

template<class T>
auto f() {
  return []<typename U>(U) {
    return typename A<T*, U&>::type{}(true);
  }(0);
}

using type = decltype(f<void>());
using type = W<void*, int&, bool>;
