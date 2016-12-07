// PR c++/72808
// { dg-do compile }
// { dg-options "-g -std=c++14" }

struct A
{
  virtual void foo ();
};

struct B : A
{
  void foo ();
  enum C : int;
};

enum B::C : int
{
  D
};

void
B::foo ()
{
}
