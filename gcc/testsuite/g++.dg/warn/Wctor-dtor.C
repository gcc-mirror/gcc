// PR c++/21347
// { dg-options "-Wctor-dtor-privacy" }

class A {
public:
  int x;
  int getX() { return x; } // comment out to avoid warning
};

int foo() {
  A a; // accepted: clearly the ctor is not private
  return a.x;
}
