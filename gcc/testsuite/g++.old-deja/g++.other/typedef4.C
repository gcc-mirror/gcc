// { dg-do assemble  }

struct A {
  void f ();
};

typedef A foo;

void foo::f() { }
