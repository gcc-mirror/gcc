// Build don't link:
// 
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 19 Jan 2001 <nathan@codesourcery.com>

// Bug 1656. We failed to make sure that a template-id was built
// from a primary template.

template <int dim> struct Outer
{
  struct Inner {};

  void f()
  {
    Inner<dim> i;         // ERROR - non-template
    Inner<> j;            // ERROR - non-template
  }
};
struct O {};
void foo ()
{
  Outer<1> x;
  x.f ();
  Outer<1>::Inner<2> z;   // ERROR - non-template
  O<1> w;                 // ERROR - non-template
}

template <typename T, template <typename C> class TPL>
struct X
{
  TPL<T> t;
  T<int> s;     // ERROR - non-template
};

template <typename T> struct Y
{
};

void bar ()
{
  X<int, Y> a;
  X<int, O> b;  // ERROR - non-template
}
