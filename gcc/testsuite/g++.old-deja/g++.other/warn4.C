// { dg-do assemble  }

// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 21 Nov 1999 <nathan@acm.org>

// make sure we only warn on assigning a negative (signed) value
// to an unsigned type, and don't warn just if the unsigned value
// happens to have the top bit set.

typedef unsigned U;

void fn (unsigned);

void fu ()
{
  unsigned s1 = -1u;
  unsigned s2(-1u);
  unsigned volatile s3 = -1u;
  unsigned const s4 = -1u;
  unsigned const &s5(-1u);
  s1 = -1u;
  fn (-1u);
}

void fU ()
{
  U s1 = -1u;
  U s2(-1u);
  U volatile s3 = -1u;
  U const s4 = -1u;
  U const &s5(-1u);
  s1 = -1u;
  fn (-1u);
}

void fs ()
{
  unsigned s1 = -1;         // { dg-warning "" } initialization
  unsigned s2(-1);          // { dg-warning "" } initialization
  unsigned volatile s3 = -1;// { dg-warning "" } initialization
  unsigned const s4 = -1;   // { dg-warning "" } initialization
  unsigned const &s5(-1);   // { dg-warning "" } initialization
  s1 = -1;                  // { dg-warning "" } assignment
  fn (-1);                  // { dg-warning "" } passing
}

void fss ()
{
  unsigned s1 = -(-1);
  unsigned s2(-(-1));
  unsigned volatile s3 = -(-1);
  unsigned const s4 = -(-1);
  unsigned const &s5(-(-1));
  s1 = -(-1);
  fn (-(-1));
}

void fsz ()
{
  unsigned s1 = -0;
  unsigned s2(-0);
  unsigned volatile s3 = -0;
  unsigned const s4 = -0;
  unsigned const &s5(-0);
  s1 = -0;
  fn (-0);
}
