// { dg-do run }

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 15 Sep 2002 <nathan@codesourcery.com>

// PR 7919. Methods found via using decls didn't have their this
// pointers converted to the final base type.

struct Base {
  int m;
  protected:
  void *Return () { return this; }
};

struct Derived : Base {
  using Base::Return;
  virtual ~Derived () {}
};

int main ()
{
  Derived d;
  
  return static_cast <Base *> (&d) != d.Return ();
}
