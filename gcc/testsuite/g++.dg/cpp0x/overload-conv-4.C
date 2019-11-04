// PR c++/90998 - ICE with copy elision in init by ctor and -Wconversion.
// { dg-do compile { target c++11 } }
// { dg-options "-Wconversion" }

struct B;

struct A {
    operator B();
};

struct B {
    B(A const &rs);
    B(B const &rs);
};

B
f (A x)
{
  // C++14: we call B::B(A const &)
  // C++17: we call A::operator B()
  return B(x); // { dg-warning "choosing .A::operator B\\(\\). over .B::B\\(const A&\\)" "" { target c++17 } }
  // { dg-warning "for conversion from .A. to .B." "" { target c++17 } .-1 }
}
