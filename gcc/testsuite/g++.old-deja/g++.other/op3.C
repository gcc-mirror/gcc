// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 28 Nov 2000 <nathan@codesourcery.com>

// Related to bug 91. We'd not preserve constness accessing a member of the
// source type in copy ctor and assignment op.

#include <stdio.h>

int glob = 0;

struct A
{
  A() {}

  A( A& arg) 
  { printf ("%s\n", __PRETTY_FUNCTION__); glob = 1;}

  A( const A& arg)
  { printf ("%s\n", __PRETTY_FUNCTION__); glob = 2;}

  A& operator=( A& ) 
  { printf ("%s\n", __PRETTY_FUNCTION__); glob = 3; return *this; }

  A& operator=( const A& ) 
  { printf ("%s\n", __PRETTY_FUNCTION__); glob = 4; return *this; }
};

struct B
{
  A a;
  B () {}
};

void foo( A& )
{
  printf ("%s\n", __PRETTY_FUNCTION__); glob = 5;
}

void foo( const A& )
{
 printf ("%s\n", __PRETTY_FUNCTION__); glob = 6;
}

int main()
{
  const A a0;
  glob = 0; printf ("A(cA) : ");  A a1(a0); if (glob != 2) return 1;
  glob = 0; printf ("A(A ) : ");  A a2(a1); if (glob != 1) return 2;
  
  const B b0;
  glob = 0; printf ("B(cB) : ");  B b1(b0); if (glob != 2) return 3;
  glob = 0; printf ("B(B ) : ");  B b2(b1); if (glob != 2) return 4;

  glob = 0; printf ("A= cA : ");  a1 = a0; if (glob != 4) return 5;
  glob = 0; printf ("A= A : ");   a1 = a2; if (glob != 3) return 6;
  glob = 0; printf ("B= cB : ");  b1 = b0; if (glob != 4) return 7;
  glob = 0; printf ("B= B : ");   b1 = b2; if (glob != 4) return 8;

  glob = 0; printf ("foo(cB): "); foo(b0.a); if (glob != 6) return 9;
  glob = 0; printf ("foo(B ): "); foo(b2.a); if (glob != 5) return 10;

  return 0;
}
