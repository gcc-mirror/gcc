// { dg-do assemble  }

struct A { typedef int foo; };
struct B: public A {
  typedef int bar;
  struct C {
    void g (B::bar);		// { dg-bogus "" } nested type failure
    void f (B::foo);
  };
};
