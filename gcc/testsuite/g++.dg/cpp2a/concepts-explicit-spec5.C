// { dg-do compile { target c++20 } }

#include <cassert>

template<typename T>
  concept C = __is_class(T);

template<typename T>
  concept D = C<T> && __is_empty(T);

struct X { } x;
struct Y { int n; } y;

int called = 0;

template<typename T>
  struct S {
    void f() requires C<T>;
  };

template<> void S<int>::f() { called = 1; } // { dg-error "match" }
