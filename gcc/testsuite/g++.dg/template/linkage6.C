// PR c++/118849
// { dg-do compile { target c++20 } }
// { dg-final { scan-assembler-not "(weak|glob)\[^\n\]*_Z" { xfail powerpc-*-aix* } } }

namespace {
  struct A {};
}

template <auto Q> void f() { }

int main() {
  f<A{}>();
}
