// PR c++/126860

struct A {
  int (*foo)();
};

template <typename T> struct S : public A {
  void bar() { A::foo(); }
};
