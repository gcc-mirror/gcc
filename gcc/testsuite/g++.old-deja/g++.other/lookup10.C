// { dg-do assemble  }

struct A {
  struct B {
    static int i;
  };
  enum { a };
};

int A::B::i = a;
