// { dg-do compile }
// The main function shall not be declared with a linkage-specification
// other than "C++".

extern "C" {
  int main();  // { dg-error "linkage" }
}

namespace foo {
  extern "C" int main();  // { dg-error "linkage" }
}

extern "C++" int main();

extern "C" struct S { int main(); };  // OK
