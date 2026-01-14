// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test from [over.match.viable].
// FIXME We should give the errors below.

namespace A {
  extern "C" void f(int = 5);
}
namespace B {
  extern "C" void f(int = 5);
}

void splice() {
  [:^^A::f:](3);  // OK, default argument was not used for viability
  [:^^A::f:]();   // error: found default argument twice
}

using A::f;
using B::f;

void use() {
  f(3);           // OK, default argument was not used for viability
  f();            // error: found default argument twice
}
