// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test dependent namespaces.

using info = decltype(^^int);

template <info R> int fn() {
  return [:R:]::v;
}

namespace NS {
  int v = 1;
}

int a = fn<^^NS>();
