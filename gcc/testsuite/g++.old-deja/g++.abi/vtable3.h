// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 4 February 2001 <nathan@codesourcery.com>

// Check constructor vtables work. This is included from numerous test
// files, which set the #defines necessary to specify the hierarchy.

#include <typeinfo>
#include <stdio.h>

int fail;
struct A;

template <typename BASE, typename DERIVED>
int Test (DERIVED *d, int expect)
{
  BASE *b = static_cast <BASE *> (d);
  void *full_b = dynamic_cast <void *> (b);
  void *full_d = dynamic_cast <void *> (d);
  A *ap = static_cast <A *> (b);
  
  if (full_b != full_d)
    {
      fail++;
      fprintf (stderr, "base %s and derived %s have different full objects\n",
              typeid (BASE).name (), typeid (DERIVED).name ());
      return 1;
    }

  DERIVED *dynamic_d = dynamic_cast <DERIVED *> (b);
  
  if (dynamic_d != d)
    {
      fail++;
      fprintf (stderr, "dynamic_cast from %s to %s failed\n",
              typeid (BASE).name (), typeid (DERIVED).name ());
      return 1;
    }

  b->Baz (static_cast <void *> (ap));
  
  int res = b->Foo (static_cast <void *> (d));
  
  if (res != expect)
    {
      fail++;
      fprintf (stderr, "%s::Foo returned %d, expected %d\n",
              typeid (BASE).name (), res, expect);
      return 1;
    }

  return 0;
}

template <typename T>
int Test (T *self, void *expected, int result)
{
  if (self != expected)
    {
      fail++;
      fprintf (stderr, "%s::Foo wrong this pointer\n", typeid (T).name ());
    }
  return result;
}

struct A {
#ifndef A_EMPTY
  int a_m;
#endif
  virtual int Foo (void *p) {return Test (this, p, 1);}
  virtual int Baz (void *p) {return Test (this, p, 1);}
  A ();
  ~A ();
};

struct B1: virtual A {
#ifndef B1_EMPTY
  int b1_m;
#endif
  virtual int Foo (void *p) {return Test (this, p, 2);}
  B1();
  ~B1();
};

struct B2: virtual A {
#ifndef B2_EMPTY
  int b2_m;
#endif
  virtual int Foo (void *p) {return Test (this, p, 3);}
  B2();
  ~B2();
};

struct Empty {};

struct C : C_PARENTS {
#ifndef C_EMPTY
  int c_m;
#endif
  virtual int Foo (void *p) {return Test (this, p, 4);}
  C();
  ~C();
};

A::A ()
{
  fprintf (stderr, "%s\n", __PRETTY_FUNCTION__);
  Test <A> (this, 1);
}
A::~A ()
{
  fprintf (stderr, "%s\n", __PRETTY_FUNCTION__);
  Test <A> (this, 1);
}

B1::B1()
{
  fprintf (stderr, "%s\n", __PRETTY_FUNCTION__);
  Test <A> (this, 2);
  Test <B1> (this, 2);
}
B1::~B1()
{
  fprintf (stderr, "%s\n", __PRETTY_FUNCTION__);
  Test <A> (this, 2);
  Test <B1> (this, 2);
}
B2::B2()
{
  fprintf (stderr, "%s\n", __PRETTY_FUNCTION__);
  Test <A> (this, 3);
  Test <B2> (this, 3);
}
B2::~B2()
{
  fprintf (stderr, "%s\n", __PRETTY_FUNCTION__);
  Test <A> (this, 3);
  Test <B2> (this, 3);
}
C::C()
{
  fprintf (stderr, "%s\n", __PRETTY_FUNCTION__);
  Test <A> (this, 4);
  Test <C> (this, 4);
}
C::~C()
{
  fprintf (stderr, "%s\n", __PRETTY_FUNCTION__);
  Test <A> (this, 4);
  Test <C> (this, 4);
}

struct D : C {};
struct D1 : virtual C {};
struct D2 : virtual A, virtual C {};

int main()
{
  {
    fprintf (stderr, "C\n");
    C c;
  }
  {
    fprintf (stderr, "D\n");
    D d;
  }
  {
    fprintf (stderr, "D1\n");
    D1 d1;
  }
  {
    fprintf (stderr, "D2\n");
    D2 d2;
  }
  if (fail)
    fprintf (stderr, "There are %d failings\n", fail);
  else
    fprintf (stderr, "Passed\n");
  return fail ? 1 : 0;
}
