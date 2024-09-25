// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }
// { dg-skip-if "requires hosted libstdc++ for cassert" { ! hostedlib } }

#include <cassert>

// Check partial ordering during overload resolution.

template<typename T>
  concept C = __is_class(T);

template<typename T>
  concept D = C<T> and __is_empty(T);

struct S1 { } s1;
struct S2 { int n; } s2;

int called = 0;

template<C T> void f1(T x) { called = 1;}
template<D T> void f1(T x) { called = 2;}

int main() {
  f1(s1); assert(called == 2);
  f1(s2); assert(called == 1);
}
