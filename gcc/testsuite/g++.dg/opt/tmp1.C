// { dg-do run }


// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 30 Jul 2003 <nathan@codesourcery.com>

// compound exprs were causing additional temporaries.

extern "C" int printf (char const *, ...);
extern "C" void abort ();


static unsigned order[] = 
{
  1, 2, 502, 102, 101,
  0
};

static unsigned point;

static void Check (unsigned t, unsigned i, void const *ptr, char const *name)
{
  printf ("%d %d %p %s\n", t, i, ptr, name);
  
  if (order[point++] != i + t)
    abort ();
  
}

template <int I> struct A 
{
  A () { Check (0, I, this, __PRETTY_FUNCTION__); }
  ~A () { Check (100, I, this, __PRETTY_FUNCTION__); }
  A (A const &) { Check (200, I, this, __PRETTY_FUNCTION__); }
  A &operator= (A const &) { Check (300, I, this, __PRETTY_FUNCTION__); }
  void Foo () const { Check (400, I, this, __PRETTY_FUNCTION__); }
};

template <int I> void Foo (A<I> a)
{
  Check (500, I, &a, __PRETTY_FUNCTION__);
}

int main ()
{
  Foo ((A<1> (), A<2> ()));
  Check (0, 0, 0, "end");
}
