struct A {
  A ();
};

struct B {
  A a1;
  A a2;
};

A a;

struct B b[] = { { a, a }, { a, a } };
