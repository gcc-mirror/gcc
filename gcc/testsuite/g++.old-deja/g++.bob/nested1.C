// Build don't link: 

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
