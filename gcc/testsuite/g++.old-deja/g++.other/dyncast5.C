// { dg-do run  }
// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 6 Jun 1999 <nathan@acm.org>

// dynamic cast can only cast to public unambiguous bases

extern "C" void abort ();

struct A {virtual ~A(){} int m; };
struct B {virtual ~B(){} int m; };

struct C1 : A {int m;};
struct C2 : A {int m;};

// A is ambiguous, but private in one branch
struct D1 : B, C1, private C2 {int m;};
// A is ambiguous, and public in both branches
struct D2 : B, C1, C2 {int m;};

void fn(B *bd1, B *bd2)
{
  A *ad1;
  A *ad2;
  
  ad1 = dynamic_cast<A *>(bd1);
  if(ad1) abort();
  ad2 = dynamic_cast<A *>(bd2);
  if(ad2) abort();
}

int main()
{
  D1 d1;
  D2 d2;
  
  fn((B *)&d1, (B *)&d2);
  return 0;
}
