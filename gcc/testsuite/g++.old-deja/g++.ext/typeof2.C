// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

struct S
{
  int i;
  __typeof( S::i ) f ();
};
