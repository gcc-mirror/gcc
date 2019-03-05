// { dg-do run { target c++17 } }
// { dg-options "-fconcepts" }

#include <cassert>
#include <iostream>

template<typename T>
  concept bool C1 = __is_class(T);

template<typename T>
  concept bool C2() { return __is_class(T); }

template<typename T>
  concept bool C3() { return requires (T a) { ++a; }; }

int main() {
  if (C1<int>) assert(false);
  if (C2<int>()) assert(false);
  if (!C3<int>()) assert(false);
}
