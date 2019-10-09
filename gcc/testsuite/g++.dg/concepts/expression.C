// { dg-do run { target c++17_only } }
// { dg-options "-fconcepts" }

// TODO: ICE on gimplify 16?

#include <cassert>
#include <iostream>

template<typename T>
  concept bool C1 = __is_class(T);

template<typename T>
  concept bool C3 = requires (T a) { ++a; };

int main() {
  if (C1<int>) assert(false);
  if (!C3<int>) assert(false);
}
