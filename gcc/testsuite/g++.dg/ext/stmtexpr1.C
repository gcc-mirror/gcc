// { dg-do run }
// { dg-options "" }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 30 Jul 2003 <nathan@codesourcery.com>

// make statement expressions work properly

extern "C" int printf (char const *, ...);
extern "C" void abort ();

static unsigned order[] = 
{
  1, 101, 2, 102,
  3, 4, 104, 103,
  5, 6, 105, 106,
  7, 107, 8, 408, 9, 109, 108,
  10, 11, 110, 411, 12, 112, 111,
  13, 113,
  14, 214, 114, 114,
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

int main ()
{
  ({A<1> (); A<2> (); ;});
  ({A<3> (), A<4> (); ;});
  ({A<5> (), A<6> ();});
  ({A <7> (); A<8> (); }).Foo (), A<9> ();
  ({A <10> (), A<11> (); }).Foo (), A<12> ();
  ({A<13> a; a; ; });
  ({A<14> a; a; });
  Check (0, 0, 0, "end");
}

