// Test whether -Wunused handles effectless indirect_ref operation ('*').
// { dg-do compile }
// { dg-options "-Wunused" }

void Foo(int* x) {
  *x++; // { dg-warning "value computed is not used" }
}
