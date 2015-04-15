// { dg-do assemble  }

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 24 Nov 2000 <nathan@codesourcery.com>

// Bug 853: We reported the wrong line no for a friend access violation

class F
{
  class Internal;   // { dg-message "" } private
};

class C
{
  friend class F::Internal; // { dg-error "" } in this context
  public:
  typedef enum { A, B } e;

  C ();
  ~C();

  void m();
};
