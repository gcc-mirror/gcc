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
  A(c.p.i); // { dg-error "member.*non-class" }
  return 0;
}

