// { dg-do run  }
// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 5 Jun 2001 <nathan@codesourcery.com>

// Bug 3006. Constructor vtables were wrong.

struct A
{
  virtual ~A() {}
};

class B : public virtual A {};
class C : public virtual B {};
class D1 : public virtual C {};
class D2 : public virtual C {};
class E
  : public virtual D1,
    public virtual D2
{
};


int
main(int argc, char* argv[])
{
  new E;
  return 0;
}
