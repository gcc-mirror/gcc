// PR c++/14278
// { dg-options "" }

struct X { 
  X (int p);
};

struct A {
  A(X);
};

void *p_fun;

A a(X ((*(int (*)(int, int)) p_fun)(0, 0)));
