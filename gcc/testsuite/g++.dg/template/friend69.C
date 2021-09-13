// { dg-do compile }

struct A;

struct B {
  friend struct A;
private:
  static void f();
protected:
  static void g();
};

struct A {
  friend void g(A) {
    B::f(); // { dg-error "private" }
    B::g(); // { dg-error "protected" }
  }
};
