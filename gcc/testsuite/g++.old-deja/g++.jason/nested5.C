// { dg-do assemble  }

struct A {
  struct B { };
};

struct C : public A {
  struct D
    : public B			// { dg-bogus "" } can't find B
    { };
};
