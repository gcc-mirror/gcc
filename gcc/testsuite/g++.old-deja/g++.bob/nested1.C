// { dg-do assemble  }

struct A {
  struct B {
    typedef long T;
    int i;
  };
};
struct C {
  struct B {
    typedef float T;
    int i;
  };
};

C::B::T a;
