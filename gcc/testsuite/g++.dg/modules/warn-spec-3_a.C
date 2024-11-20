// { dg-do compile { target c++20 } }
// { dg-additional-options -fmodules }
// { dg-module-cmi M }

module;

#include <initializer_list>

export module M;

#pragma GCC diagnostic ignored "-Winit-list-lifetime"

template <class T>
struct myspan {
  const T* p; unsigned s;
  myspan (std::initializer_list<T> il)
    : p (il.begin()), s (il.size()) { }
};

export void f(myspan<int>);
