// Build don't run
// 
// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 12 Jun 2001 <nathan@codesourcery.com>

// 3132. A virtual thunk was missing.

struct A
{
  int x;
  virtual ~A() {}
};

struct B
{
  virtual ~B() { }
};


struct C
{
  virtual ~C () {}
};


struct D : public virtual A {};
struct E : virtual public B, virtual public D {};
struct F : virtual public C, virtual public E {};
struct G : public virtual E {};

struct H : public virtual F, public virtual G
{
  virtual ~H ();
};
H::~H() {}

int main ()
{
  return 0;
}
