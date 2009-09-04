/* { dg-do compile } */

struct A
{
  virtual void foo();
  virtual ~A();
  int i;
};

struct B : virtual A {};

struct C : B
{
  virtual void foo();
};

void bar()
{
  C().foo();
}
