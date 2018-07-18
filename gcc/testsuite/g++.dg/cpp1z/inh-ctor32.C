// { dg-do compile { target c++11 } }

struct B;
struct A
{
  A(const B&, int = 0);
};

struct B: A
{
  using A::A;
  B(B&);
};

extern const B b;
B b2{b};			// { dg-error "" }
