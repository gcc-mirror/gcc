// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 17 Dec 2004 <nathan@codesourcery.com>

// PR 18975: Rejects legal
// Origin:   Wolfgang Roehrl <wolfgang.roehrl@de.gi-de.com>

struct PTR
{
  PTR ();
  PTR (PTR&);
  PTR& operator= (PTR&);
  
private:
  PTR (const PTR&);
  PTR& operator= (const PTR&);
};


struct XYZ
{
  XYZ (PTR& p) : ptr(p) {}

  mutable PTR ptr;
};


XYZ f1 ();


XYZ f2 (void) { return f1(); }
void f3 (XYZ& dst, const XYZ& src) { dst = src; }
