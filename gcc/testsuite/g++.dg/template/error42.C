// PR c++/40372
// { dg-do compile }

template <int> struct A
{
  int i;		// { dg-error "invalid use of non-static data member" }
  friend void foo ()
  {
    int x[i];		// { dg-error "from this location" }
  }
};

struct B
{
  int j;		// { dg-error "invalid use of non-static data member" }
  friend int bar ()
  {
    return j;		// { dg-error "from this location" }
  }
};
