// Adapted from g++.old-deja/g++.law/arm8.C

struct K {
  void f(int);
};

void K::f(int);  // { dg-error "6:declaration of .void K::f\\(int\\). outside of class" }

struct L {
  void g(int) {}
};

void L::g(int);  // { dg-error "6:declaration of .void L::g\\(int\\). outside of class" }
