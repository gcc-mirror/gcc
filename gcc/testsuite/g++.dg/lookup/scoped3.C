
// { dg-do compile }

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 24 Dec 2002 <nathan@codesourcery.com>

// PR 7964. ICE with scoped method call

struct A {
  virtual void ostr() const;
};

class B : public virtual A {};

template<typename T>
struct C : public B
{
  void ostr() const
  {    B::ostr();  }
};


template C<int>;
