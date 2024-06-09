// { dg-do compile }
// A program that declares an entity named main with C language linkage
// (in any namespace) is ill-formed.

namespace foo {
  extern "C" int main;  // { dg-error "linkage" }
  extern "C" struct A {
    int main;  // OK
  };
  extern "C" struct B {
    int main();  // OK
  };
}
