// { dg-do assemble  }

struct S {
  int A;
  struct A {
    enum { a = 0 };
  };

  void f();
};

void S::f() {
  A = A::a;
}
