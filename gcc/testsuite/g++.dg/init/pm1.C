// { dg-do run }

// Copyright 2002  Free Software Foundation
// Contributed by Jason Merrill and Alexandre Oliva

// Test zero-initialization of pointers to data members.  Their NULL
// value is represented with -1, not 0.

#include <stdlib.h>

struct A
{
  int i;
};

int A::* gp;

typedef int A::* iApm;

iApm gp_zero = 0;
iApm gp_dflt = iApm();
iApm gp_cast = (iApm)0;
iApm gp_func = iApm(0);
iApm gp_stat = static_cast<iApm>(0);

struct AD : A {};

int AD::* gp_impl = gp_dflt;
int AD::* gp_down = static_cast<int AD::*>(gp_stat);

int A::* ga[2];

// Test use in a simple struct.
struct B
{
  int A::* mp;
};

B gb;

struct D;
struct C;
extern D gd;
extern C gc;

// Test that in a class with a constructor, the pointer to member is
// zero-initialized until the constructor is run.
struct C
{
  int A::* mp;
  inline C ();
};

int fail;
struct D
{
  int count;
  inline D ();
};

C::C() : mp (&A::i) { gd.count++; }

D::D() : count (0)
{
  if (gc.mp != 0)
    abort ();
}

// The D must come first for this to work.
D gd;
C gc;

int main()
{
  static int A::* slp;
  static int A::* sla[2];
  static B slb;

  if (gp != 0 || slp != 0
      || gp_zero != 0 || gp_dflt != 0 || gp_cast != 0
      || gp_func != 0 || gp_stat != 0
      || gp_impl != 0 || gp_down != 0)
    abort ();
  if (ga[1] != 0 || sla[1] != 0)
    abort ();
  if (gb.mp != 0 || slb.mp != 0)
    abort ();
}
