// { dg-do assemble  }

struct B { 
  int i;
};

struct D: virtual public B {
  int i;
};

struct D2 : public D {
  void f() { i = 3; }
};
