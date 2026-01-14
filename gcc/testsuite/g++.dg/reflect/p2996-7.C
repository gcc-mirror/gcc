// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test from [temp.dep.namespace].

using info = decltype(^^int);

template <info R> int fn() {
  namespace Alias = [:R:];  // [:R:] is dependent
  return Alias::v;  // Alias is dependent
}

namespace NS {
  int v = 1;
}

int a = fn<^^NS>();
