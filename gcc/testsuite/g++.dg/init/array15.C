// { dg-do run }
// { dg-require-effective-target size24plus }

// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 8 Dec 2004 <nathan@codesourcery.com>

// PR 16681 too much memory used
// Origin:  Matt LaFary <lafary@activmedia.com>

struct foo {
  unsigned char buffer[4111222];
  foo() ;
  bool check () const;
};

foo::foo ()
  : buffer()
{}

bool foo::check () const
{
  for (unsigned ix = sizeof (buffer); ix--;)
    if (buffer[ix])
      return false;
  return true;
}

void *operator new (__SIZE_TYPE__ size, void *p)
{
  return p;
}

char heap[5000000];

int main ()
{
  for (unsigned ix = sizeof (heap); ix--;)
    heap[ix] = ix;
  
  foo *f = new (heap) foo ();

  if (!f->check ())
    return 1;
  return 0;
}

  
