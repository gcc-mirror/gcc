// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

#include <cassert>

template<typename T>
  concept bool C() { return __is_class(T); }

template<typename T>
  concept bool D() { return C<T>() && __is_empty(T); }

struct X { } x;
struct Y { int n; } y;

int called = 0;

template<typename T>
  struct S {
    void f() requires C<T>();
  };

template<> void S<int>::f() { called = 1; } // { dg-error "match" }
