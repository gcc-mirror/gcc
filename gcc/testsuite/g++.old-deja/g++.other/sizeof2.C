// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

struct S
{
  int j; // ERROR - non-static data member
  int i[2]; // ERROR - non-static data member
};

void f ()
{
  sizeof (S::j); // ERROR - used here
  sizeof (S::i[0]); //  ERROR - used here
}

