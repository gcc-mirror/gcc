// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }
// { dg-skip-if "requires hosted libstdc++ for cassert" { ! hostedlib } }

#include <cassert>

template<typename T>
  concept Class = __is_class(T);

template<typename T>
  concept Empty = Class<T> and __is_empty(T);

template<Class T> int f(T) { return 1; }
template<Empty T> int f(T) { return 2; }

struct S {
  template<Class T> int f(T) { return 1; }
  template<Empty T> int f(T) { return 2; }
} s;

struct X { } x;
struct Y { X x; } y;

int main () {
  auto p1 = &f<X>; // Empty f
  assert(p1(x) == 2);

  auto p2 = &f<Y>; // Class f
  assert(p2(y) == 1);

  auto p3 = &S::template f<X>; // Empty f
  assert((s.*p3)(x) == 2);

  auto p4 = &S::template f<Y>; // Empty f
  assert((s.*p4)(y) == 1);
}
