// Build don't link:

struct A { typedef int foo; };
struct B: public A {
  typedef int bar;
  struct C {
    void g (B::bar);		// gets bogus error - nested type failure
    void f (B::foo);
  };
};
