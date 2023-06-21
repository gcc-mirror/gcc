// PR c++/58487
// { dg-additional-options -Wnrvo }

struct A {
  A() {}
  A(const A&);
};

A test() {
  if (true) {
    return A();
  } else {
    A a;
    return a;		       // { dg-bogus "not eliding" "" { xfail *-*-* } }
  }
}

int main() { }
