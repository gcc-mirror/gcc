// Build don't link:
// 
// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 12 Jun 2001 <nathan@codesourcery.com>

// 3131. 

struct A
{
  virtual ~A ();
};

struct B
{
  virtual ~B ();
};


struct C : virtual public B {};
struct D : virtual public A, virtual public C {};
struct E : public virtual C {};
struct F : public virtual D, public virtual E {};
