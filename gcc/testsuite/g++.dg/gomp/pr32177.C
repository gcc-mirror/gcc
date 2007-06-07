// PR c++/32177
// { dg-do compile }
// { dg-options "-fopenmp" }
//
// Copyright (C) 2007 Free Software Foundation, Inc.
// Contributed by Theodore.Papadopoulo 1 Jun 2007 <Theodore.Papadopoulo@sophia.inria.fr>

struct A
{
  A () {}
  ~A () {}
  int s () const { return 1; }
};

void
f1 ()
{
  #pragma omp parallel for
    for (int i = 1; i <= A ().s (); ++i)
      ;
}

void
f2 ()
{
  #pragma omp parallel for
    for (int i = A ().s (); i <= 20; ++i)
      ;
}

void
f3 ()
{
  #pragma omp parallel for
    for (int i = 1; i <= 20; i += A ().s ())
      ;
}

void
f4 ()
{
  int i;
  #pragma omp parallel for
    for (i = A ().s (); i <= 20; i++)
      ;
}
