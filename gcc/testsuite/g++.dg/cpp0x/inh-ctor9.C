// { dg-do compile { target c++11 } }

class A
{
  int i;
protected:
  A(int i): i(i) {}
};

struct B: A
{
  using A::A;
};

B b(42);			// { dg-error "this context" }
