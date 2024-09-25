// { dg-do run { target c++17 } }
// { dg-options "-fconcepts" }
// { dg-skip-if "requires hosted libstdc++ for cassert" { ! hostedlib } }

#include <cassert>
#include <iostream>

template<typename T>
  concept C1 = __is_class(T);

template<typename T>
  concept C3 = requires (T a) { ++a; };

int main() {
  if (C1<int>) assert(false);
  if (!C3<int>) assert(false);
}
