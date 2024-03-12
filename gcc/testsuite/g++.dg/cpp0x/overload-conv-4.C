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
  // C++17: now the same
  return B(x);
}
