// { dg-do compile }
// { dg-options "-fshow-column" } 
template<typename t>struct foo {
  static const int i; };

const int foo<bool>::i = 5; // { dg-error "11:specializing member .foo<bool>::i. requires .template<>. syntax" }

int main() { return 0; }

