// Build don't link:

struct A {
  struct B { };
};

struct C : public A {
  struct D
    : public B			// gets bogus error - can't find B
    { };
};
