// { dg-do run }
// { dg-options "-O2" }

// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 2 Dec 2004 <nathan@codesourcery.com>

// PR 18318. ICE with template new[]
// Origin:Elliot Hughes <enh@jessies.org>
// Andrew Pinski <pinskia@gcc.gnu.org>

struct Aint
{
  ~Aint ();
  Aint ();
};

Aint::Aint () {}
Aint::~Aint () {}

static int count;

template <class T>
struct A
{
  unsigned Blksize() const;
  
  void f()
  {
    new T[Blksize()];
  }
};

template <class T> unsigned A<T>::Blksize () const
{
  count++;
  return 1;
}

int main ()
{
  A<Aint> a;
  a.f();
  
  return count != 1;
}
