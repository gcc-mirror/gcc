// { dg-do compile }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 26 Dec 2001 <nathan@codesourcery.com>

// PR 35. We were default promoting template PARM_DECLs

template <short B> class R {};

template <class T> class A
{
  public:
  template <short B>
  void operator() (R<B> const &);
};

int main() {
  A<int> a;
  R<1> r;

  a (r);
  
  return 0;
}
