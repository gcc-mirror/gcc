// PR c++/108559
// { dg-do compile { target c++17 } }

struct A {
  int g = 0;

  A() {}
  A(const A&) {}
};

struct B : A {};

A u() { return A{}; }

int bug() { return B{u()}.g; }

int main() {
  return 0;
}
