// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

using namespace std::meta;

struct A { int a; };
struct B { int b; };
struct C : A, B { };
struct D : A, B { };

constexpr access_context uctx = access_context::unchecked ();

void
g ()
{
  A a{1};
  A &ar = a.[: bases_of(^^A, uctx)[0] :];
  C c;
  A &a2 = c.[: bases_of(^^C, uctx)[0] :];
  B &b = c.[: bases_of(^^C, uctx)[0] :]; // { dg-error "invalid initialization of reference" }
  A *ap = &c.[: bases_of(^^C, uctx)[1] :];  // { dg-error "cannot convert" }
  A &a3 = c.[: bases_of(^^D, uctx)[0] :]; // { dg-error "not a base type for type" }
  B &b2 = c.[: bases_of(^^D, uctx)[1] :]; // { dg-error "not a base type for type" }
}

// { dg-error "call to non-.constexpr." "" { target *-*-* } 0 }
