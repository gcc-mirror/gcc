// { dg-do run }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 23 Jan 2003 <nathan@codesourcery.com>

// PR9415. Forgot a lookup was scoped

int here;
int there;

struct B
{
  virtual int activate() {return !here++;}
};

template <class K>
struct TPL : public B
{
  int activate()
  {
    return !there++ && B::activate();
  }
};

int main ()
{
  TPL<int> i;
  return !i.activate ();
}
