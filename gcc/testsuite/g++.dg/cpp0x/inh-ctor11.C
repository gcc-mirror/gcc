// { dg-options "-std=c++11" }

struct A
{
  A(int, ...);
};

struct B: A
{
  using A::A;
};

B b1(42);
B b2(42, 1.0);			// { dg-error "no match" }
