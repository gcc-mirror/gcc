// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

struct S
{
  int i; // { dg-error "" } non-static data member
  __typeof( S::i ) f (); // { dg-error "" } referenced here
};
