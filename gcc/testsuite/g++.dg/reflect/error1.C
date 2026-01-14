// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

struct X { };

void
f1 ()
{
  constexpr auto r = ^^int;
  [: r ] i = 42;  // { dg-error "expected .:].|expected unqualified-id" }
  [: :] x1;	  // { dg-error "expected primary-expression|forbids|reflection not usable" }
  [: int :] x2;	  // { dg-error "expected|reflection not usable" }
  [: X :] x3;	  // { dg-error "expected|reflection not usable" }
  constexpr X x;
  [: x :] x4;	  // { dg-error "could not convert .x. from .const X. to .std::meta::info.|reflection not usable" }

  constexpr auto e1 = ^^42;  // { dg-error "cannot be applied" }
}
