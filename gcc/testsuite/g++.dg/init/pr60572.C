// PR c++/60572
// { dg-do compile }

struct A
{
  A x;	// { dg-error "incomplete type" }
  virtual ~A () {}
};

struct B : A
{
  B () : A () {}
};
