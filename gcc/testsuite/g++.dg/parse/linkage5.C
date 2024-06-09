// { dg-do compile }
// The main function shall not be declared with a linkage-specification.

extern "C" {
  int main();  // { dg-error "linkage" }
}

namespace foo {
  extern "C" int main();  // { dg-error "linkage" }
}

extern "C++" int main(); // { dg-error "linkage" }

extern "C" struct S { int main(); };  // OK
