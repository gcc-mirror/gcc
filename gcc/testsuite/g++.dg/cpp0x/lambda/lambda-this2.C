// PR c++/43856
// Test for implicit 'this' capture via rewriting.
// { dg-do compile { target c++11 } }

struct S1 {
  int operator()(int);
  int i;
  void g();
  void f() {
    [=]() {
      i;
      g();
      S1::g();
      operator()(42);
    };
  }
};
