struct A1 {
  explicit A1(int) { }
} a1(1);

struct A {
  explicit A(int);
} a(1);

A::A(int) {
}

void foo(A a) {
  foo(a);
  foo(1);		// ERROR - not allowed
}
