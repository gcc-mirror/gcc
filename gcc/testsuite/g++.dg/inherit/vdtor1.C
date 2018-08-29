struct A {
  void operator delete(void *, unsigned long);
};
struct B : A {
  virtual ~B();
};
struct C : B {};
