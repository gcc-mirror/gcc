// PR rtl-optimization/47366
// { dg-do compile }
// { dg-options "-O -fnon-call-exceptions -fno-tree-ccp -fno-tree-forwprop" }

struct A
{
  int i;
  virtual ~A ();
};

struct B : virtual A
{};

struct C : B
{
  void bar () {}
};

void foo ()
{
  C ().bar ();
}
