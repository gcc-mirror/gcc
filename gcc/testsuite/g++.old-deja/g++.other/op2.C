// Copyright (C) 2000, 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 28 Nov 2000 <nathan@codesourcery.com>

// Bug 91. We'd not preserve constness looking for a base classes assignment
// operator.

#include <stdio.h>

int value = 0;

struct A
{
  A() {}

  A( A& arg) 
  { printf ("%s\n", __PRETTY_FUNCTION__); value = 1;}

  A( const A& arg)
  { printf ("%s\n", __PRETTY_FUNCTION__); value = 2;}

  A& operator=( A& ) 
  { printf ("%s\n", __PRETTY_FUNCTION__); value = 3; return *this; }

  A& operator=( const A& ) 
  { printf ("%s\n", __PRETTY_FUNCTION__); value = 4; return *this; }
};

struct B : A
{
  B () {}
};

void foo( A& )
{
  printf ("%s\n", __PRETTY_FUNCTION__); value = 5;
}

void foo( const A& )
{
 printf ("%s\n", __PRETTY_FUNCTION__); value = 6;
}


int main()
{
  const A a0;
  value = 0; printf ("A(cA) : ");  A a1(a0); if (value != 2) return 1;
  value = 0; printf ("A(A ) : ");  A a2(a1); if (value != 1) return 2;
  
  const B b0;
  value = 0; printf ("B(cB) : ");  B b1(b0); if (value != 2) return 3;
  value = 0; printf ("B(B ) : ");  B b2(b1); if (value != 2) return 4;

  value = 0; printf ("A= cA : ");  a1 = a0; if (value != 4) return 5;
  value = 0; printf ("A= A : ");   a1 = a2; if (value != 3) return 6;
  value = 0; printf ("B= cB : ");  b1 = b0; if (value != 4) return 7;
  value = 0; printf ("B= B : ");   b1 = b2; if (value != 4) return 8;

  value = 0; printf ("foo(cB): "); foo(b0); if (value != 6) return 9;
  value = 0; printf ("foo(B ): "); foo(b2); if (value != 5) return 10;

  return 0;
}
