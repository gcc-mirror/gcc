// { dg-do assemble  }
// Bug: the synthesized constructor for A tries to use the mem-initializer
// list for the B constructor.

struct A
{
  virtual ~A();
};

struct B
{
  B();
  char* x;
  A* a;
};

B::B()
: x(0), a(new A())
{
}
