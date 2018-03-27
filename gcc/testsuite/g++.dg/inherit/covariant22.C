// PR c++/85068
// { dg-do compile }

struct A;

struct B
{
  virtual A *foo ();	// { dg-error "overriding" }
};

struct C : virtual B
{
  virtual C *foo ();	// { dg-error "invalid covariant return type for" }
};

struct D : C
{
  virtual C *foo ();
};
