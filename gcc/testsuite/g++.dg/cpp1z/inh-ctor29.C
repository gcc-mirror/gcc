// { dg-do compile { target c++11 } }

struct A
{
  A(int = 0);
};

struct B: A
{
  B();
  using A::A;
};

B b1(1);
B b;
