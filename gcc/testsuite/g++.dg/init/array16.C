// { dg-do run }
// { dg-options "-mstructure-size-boundary=8" { target arm-*-* } }

// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 8 Dec 2004 <nathan@codesourcery.com>

// PR 16681 too much memory used
// Origin:  Matt LaFary <lafary@activmedia.com>

// NOTE: This test assumes that 4M instances of struct ELT can fit into
//       a 5MB array.  This isn't true, e.g., with the default
//       arm-none-elf options.

struct elt 
{
  static int count;
  static elt*ptr;
  static int abort;
  char c;
  
  elt ();
  ~elt ();
  
};

int elt::count;
elt *elt::ptr;
int elt::abort;

elt::elt ()
  :c ()
{
  if (count >= 0)
    {
      if (!ptr)
	ptr = this;
      if (count == 100)
	throw 2;
      if (this != ptr)
	abort = 1;
      count++;
      ptr++;
    }
}

elt::~elt ()
{
  if (count >= 0)
    {
      ptr--;
      count--;
      if (ptr != this)
	abort = 2;
    }
}

struct foo {
  elt buffer[4111222];
  foo() ;
  bool check () const;
};

foo::foo ()
  : buffer()
{}

bool foo::check () const
{
  for (unsigned ix = sizeof (buffer)/ sizeof (buffer[0]); ix--;)
    if (buffer[ix].c)
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

  try
    {
      foo *f = new (heap) foo ();
      return 1;
    }
  catch (...)
    {
      if (elt::count)
	return 2;
      if (elt::abort)
	return elt::abort + 3;
    }

  for (unsigned ix = sizeof (heap); ix--;)
    heap[ix] = ix;

  elt::count = -1;
  foo *f = new (heap) foo ();
  if (!f->check ())
    return 3;
  return 0;
}

  
