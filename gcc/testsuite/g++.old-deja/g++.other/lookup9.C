// { dg-do assemble  }

struct S {
  typedef long I;
};

struct D : virtual public S {
  I i;
};
