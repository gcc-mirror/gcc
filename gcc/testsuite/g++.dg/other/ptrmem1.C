// { dg-do run }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 31 Dec 2001 <nathan@codesourcery.com>

// PR 4379. We created pointers to member references and pointers to
// member fields when we shouldn't have.

int gs;
int gm;

struct D {
  D () :m (gm) {}
  
  int &m;
  static int &s;
  
  int Foo ();
};

int &D::s = gs;

template<class T> int f1(T x)
{
  return x != &gm;
}
template<class T> int f2(T x) 
{
  return x != &gs;
}

int D::Foo ()
{
  int r;
  
  if (f1( &(D::m)))
    return 3;
  
  if (f2( &D::s))
    return 1;
  if (f2( &(D::s)))
    return 2;
  return 0;
}

int Foo ()
{
  if (f2( &D::s))
    return 4;
  if (f2( &(D::s)))
    return 5;
  return 0;
}

int main ()
{
  D d;
  int r = d.Foo ();
  if (r)
    return r;
  r = Foo ();
  if (r)
    return r;
  return 0;
  
}
