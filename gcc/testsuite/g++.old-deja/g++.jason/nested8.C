// { dg-do assemble  }

struct A { };
struct B: public A {
  struct C {
friend struct B;		// { dg-bogus "" } base clause w/o members
  };
};
