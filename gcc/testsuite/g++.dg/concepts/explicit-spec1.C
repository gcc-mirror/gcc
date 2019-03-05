// { dg-do run { target c++17 } }
// { dg-options "-fconcepts" }

#include <cassert>

template<typename T>
  concept bool C() { return __is_class(T); }

template<typename T>
  concept bool D() { return C<T>() && __is_empty(T); }

struct X { } x;
struct Y { int n; } y;

template<typename T> void g(T) { } // #1
template<C T> void g(T) { }        // #2
template<D T> void g(T) { }     // #3

int called;

template<> void g(int) { called = 1; } // Specialization of #1
template<> void g<X>(X) { called = 2; } // Specialization of #3
template<> void g(Y) { called = 3; } // Specialization of #2

int main() {
  g(0);
  assert(called == 1);
  g(x);
  assert(called == 2);
  g(y);
  assert(called == 3);
}
