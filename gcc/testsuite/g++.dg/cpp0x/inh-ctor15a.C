// P0136 caused us to start inheriting base copy constructors.
// { dg-do compile { target c++11 } }
// { dg-options -fnew-inheriting-ctors }

struct A { A(int); };
struct B: public A
{
  using A::A;
};

A a (42);

B b1 (24);			// inherited
B b2 (a);			// also inherited now
