// { dg-do compile }
// PR c++/52645

class A
{
protected:
  struct B {};
};

class C : A
{
protected:
  using A::B;

  struct D : public B {};
};
