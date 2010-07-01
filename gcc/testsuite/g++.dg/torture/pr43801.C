// PR tree-optimization/43801
// { dg-do compile }
// { dg-options "-fipa-cp -fipa-cp-clone" }

struct A
{
  virtual void f (int);
};
struct B : virtual A
{
  virtual void f (int i) { if (i) A::f(0); }
};
struct C : virtual B
{
  virtual void f (int) { B::f(0); }
};

void
foo ()
{
  C ();
}
