// { dg-do compile { target c++11 } }
// { dg-options "-g" }

struct A
{
  template <class... Ts> A(Ts...);
};

struct B: A
{
  using A::A;
};

B b1(42);
B b2(1.0, 42, (void*)0);
