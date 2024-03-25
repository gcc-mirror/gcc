// PR c++/70413
// { dg-final { scan-assembler-not "(weak|glob)\[^\n\]*_Z" { xfail powerpc-*-aix* } } }

namespace {
  struct A {
    void f();
    int m;
  };
}

template<void(A::*)()> void g() { }
template<int A::*> void h() { }

int main() {
  g<&A::f>();
  h<&A::m>();
}
