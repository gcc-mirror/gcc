// PR c++/15507

struct A {
  // empty
};

struct B : A {
  int b;
};

union U {
  A a;
  B b;
};
