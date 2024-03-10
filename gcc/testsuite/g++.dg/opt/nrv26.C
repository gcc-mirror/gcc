// PR c++/58487
// { dg-additional-options -Wnrvo }
// { dg-do link }

struct A {
  A() {}
  A(const A&);
};

A test() {
  if (true) {
    A a;
    return a;
  } else {
    return A();
  }
}

int main() { }
