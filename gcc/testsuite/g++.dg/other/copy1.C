// { dg-do run }

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 30 Nov 2001 <nathan@nathan@codesourcery.com>

// PR 87

int assign = 0;
int ctor = 0;
int assignC = 0;

struct A {
  int i;

  template<class T>
  void operator=(const T&) const
  { 
    assign = 1;
  }

  A () : i (0) {}
  
  template <typename T> A (const T &)
  {
    ctor = 1;
  }
};

struct B : A 
{
};

struct C 
{
  int i;

  C (int i_) :i (i_) {}
  
  template <int I>
  void operator= (const C &)
  {
    assignC = 1;
  }
};


int main()
{
  const A a;
  A b;
  B c;

  b = a;
  if (assign)
    return 5;
  
  b.i = 100;
  c.i = 200;
  
  a = b; 

  if (!assign)
    return 1;
  if (a.i)
    return 2;

  A e (b);
  if (ctor)
    return 3;
  
  A d (c);
  if (!ctor)
    return 4;

  C c0 (0);
  C c1 (1);

  c0 = c1;
  if (assignC)
    return 5;
  
  return 0;
}
