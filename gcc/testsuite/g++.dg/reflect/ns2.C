// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test reflections on namespaces.  Invalid stuff.

void foo (int);

namespace N {
}

void
f1 ()
{
  constexpr auto r = ^^::;
  [: r :] foo (0); // { dg-error "expected" }

  constexpr auto q = ^^int;
  [: q :]::T x; // { dg-error "reflection not usable in a splice scope|expected" }
  // { dg-message ".int. is not a class, namespace, or enumeration" "" { target *-*-* } .-1 }

  constexpr auto x = ^^N::X;  // { dg-error ".N::X. has not been declared" }
}
