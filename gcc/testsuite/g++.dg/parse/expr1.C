struct A {
  A (int, int);
  void f ();
};

void f (int a) {
  A (a, a).f ();
}
