// Build don't link:

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 24 Nov 2000 <nathan@codesourcery.com>

// Bug 853: We reported the wrong line no for a friend access violation

// Since DR 209, friend declaration access is not checked.

class F
{
  class Internal;
};

class C
{
  friend class F::Internal;
  public:
  typedef enum { A, B } e;

  C ();
  ~C();

  void m();
};
