// PR c++/34068
// { dg-do compile }

template <typename> struct A
{
  typedef int X;
  A () { T (). ~X (); }	// { dg-error "there are no arguments to|fpermissive|was not declared in this scope" }
};

A <int> a;
