// Build don't link:
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 14 Nov 2000 <nathan@codesourcery.com>

// bug 616. We failed to complete the type of decls in templates, leading to
// bogus errors.

struct Z;
struct Y
{
  Y (int i = 1);
};
void g ()
{
  const Y y;
  Z z;          // ERROR - incomplete
}

template <int dim>
struct X
{
  X (int i=1);
};

void h ()
{
  const X<2> z;
  Z z1;         // ERROR - incomplete
}

template <int dim>
void f()
{
  const X<dim> x;
  const X<dim+1> y[3];
  Z z2;           // ERROR - incomplete
  typedef Z z3;   // ok
};

template void f<3> ();
