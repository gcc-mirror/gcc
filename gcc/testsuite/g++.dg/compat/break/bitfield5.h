struct A {
  virtual void f();
  int f1 : 1;
};

struct B : public A {
  int f2 : 1;
  int : 0;
  int f3 : 4;
  int f4 : 3;
};
