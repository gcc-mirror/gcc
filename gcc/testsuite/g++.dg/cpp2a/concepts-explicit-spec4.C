// { dg-do run { target c++20 } }

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
    void f() { called = 0; }                 // #1
    void f() requires C<T> { called = 0; } // #2

    void g() requires C<T> { } // #1
    void g() requires D<T> { } // #2
  };

template<> void S<int>::f() { called = 1; } // Spec of #1
template<> void S<X>::f() { called = 2; } // Spec of #2

template<> void S<X>::g() { called = 3; } // Spec of #2
template<> void S<Y>::g() { called = 4; } // Spec of #1

int main() {
  S<double> sd;
  S<int> si;
  S<X> sx;
  S<Y> sy;

  sd.f();
  assert(called == 0);
  si.f();
  assert(called == 1);
  sx.f();
  assert(called == 2);
  sy.f();
  assert(called == 0);

  sx.g();
  assert(called == 3);
  sy.g();
  assert(called == 4);
}
