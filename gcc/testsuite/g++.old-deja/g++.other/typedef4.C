// Build don't link:

struct A {
  void f ();
};

typedef A foo;

void foo::f() { }
