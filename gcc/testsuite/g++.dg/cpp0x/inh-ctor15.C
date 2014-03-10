// Discussions on the core reflector indicate that not inheriting base copy
// constructors was a deliberate choice.

// { dg-do compile { target c++11 } }

struct A { A(int); };
struct B: public A
{
  using A::A;
};

A a (42);

B b1 (24);			// inherited
B b2 (a);			// not inherited { dg-error "no match" }
