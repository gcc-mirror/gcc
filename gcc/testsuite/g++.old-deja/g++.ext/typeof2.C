// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

struct S
{
  int i; // ERROR - non-static data member
  __typeof( S::i ) f (); // ERROR - referenced here
};
