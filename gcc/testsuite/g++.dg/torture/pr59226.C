// { dg-do compile }
struct A
{
  virtual void foo() {}
};

struct B
{
  virtual void foo() {}
};

struct C : virtual A {};

struct D : virtual A, B
{
  virtual void foo() {}
};

struct E : C, D
{
  virtual void foo() {}
};

void bar(A* p)
{
  p->foo();
}
