// Build don't link:

struct A { };
struct B: public A {
  struct C {
friend struct B;		// gets bogus error - base clause w/o members
  };
};
