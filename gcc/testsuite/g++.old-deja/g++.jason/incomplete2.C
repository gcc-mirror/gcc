// Build don't link:

struct A {
  int foo(A a) { return a.bar(); }
  int bar();
  int n;
};
