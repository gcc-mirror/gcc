// { dg-options "-fshow-column -ansi -pedantic-errors -Wno-long-long" }
// PR c++/17821

struct A {
  A(int i) {}
};
struct B {
  int i;
};
struct C {
  B* p;
};
int main() {
  C c;
  A(c.p.i); // { dg-error "9:request for member 'i' in 'c.C::p', which is of pointer type 'B" }
  return 0;
}

