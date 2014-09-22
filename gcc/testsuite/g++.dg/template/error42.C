// PR c++/40372
// { dg-do compile }

template <int> struct A
{
  int i;		// { dg-message "" }
  friend void foo ()
  {
    int x[i];		// { dg-error "non-static data member" }
  }
};

struct B
{
  int j;		// { dg-message "" }
  friend int bar ()
  {
    return j;		// { dg-error "non-static data member" }
  }
};
