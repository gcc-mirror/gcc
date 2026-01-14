// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

namespace A {}
constexpr auto NS_A = ^^A;

namespace B {
  namespace [:NS_A:] {	// { dg-error "expected" }
    void fn();  // Is this '::A::fn' or '::B::A::fn' ?
  }
}
