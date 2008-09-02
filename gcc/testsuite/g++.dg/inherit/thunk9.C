// PR tree-optimization/37095
// { dg-options "-O" }

struct A
{
  virtual A *foo ();
};

struct B : virtual A
{
  virtual B *foo () { return 0; }
};

B b;
