// PR c++/69850
// { dg-do compile }
// { dg-options "-Wall" }

struct C
{
  ~C () { delete this; }	// { dg-bogus "nonnull argument" }
};
C c;
