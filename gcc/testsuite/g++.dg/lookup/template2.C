// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 24 Mar 2003 <nathan@codesourcery.com>

// PR 10199. Lookup problems

class X {
public:
  template<int d>
  int bar ();
};

template<int x>
int fooo ();

template<class T>
void bar (T& g)
{
  int kk = fooo<17>();  // OK
  X x;
  int k = x.bar<17>();  // Not OK
}

int main ()
{
  X x;
  int k=x.bar<17>();    // OK
  int n;
  bar(n);
}
