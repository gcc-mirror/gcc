// { dg-do run }
// { dg-options "-std=c++1z -fconcepts" }

#include <cassert>

// Check partial ordering during overload resolution.

template<typename T>
  concept bool C() { return __is_class(T); }

template<typename T>
  concept bool D() { return C<T>() and __is_empty(T); }

struct S1 { } s1;
struct S2 { int n; } s2;

int called = 0;

template<C T> void f1(T x) { called = 1;}
template<D T> void f1(T x) { called = 2;}

int main() {
  f1(s1); assert(called == 2);
  f1(s2); assert(called == 1);
}
