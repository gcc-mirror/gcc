// { dg-do assemble  }
// 
// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 12 Jun 2001 <nathan@codesourcery.com>

// 3130. A virtual base can have canonical and non-canonical instances
// of its primary. The canonical one should be first in dfs order.

struct A
{
  virtual ~A ();
};

struct B
{
  virtual ~B ();
};


struct C : virtual public A, virtual public B {};
class D : public virtual C {};
class E : public virtual C, public virtual D {};
