// PR debug/6387
// Verify that -g1 works with local class member functions.

void foo();
void bar() {
  struct A { A() { foo(); } } a;
}
