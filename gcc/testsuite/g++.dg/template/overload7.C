// { dg-do run }

// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 17 Oct 2005 <nathan@codesourcery.com>

// PR 24386:Wrong virtual function called
// Origin:  Scott Snyder snyder@fnal.gov

struct A
{
  virtual int Foo () { return 1; }
};
struct B : public A
{
  virtual int Foo () { return 2; }
};

template <class T>
int Bar (T *a)
{
  if (static_cast<A*>(a)->A::Foo () != 1)
    return 1;
  if (static_cast<A*>(a)->Foo () != 2)
    return 2;
  return 0;
}

int main ()
{
  return Bar (new B);
}
