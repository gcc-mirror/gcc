// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

struct S
{
  int j; // ERROR - member
  int i[2]; // ERROR - member
};

void f ()
{
  sizeof (S::j); // ERROR - non-static data member
  sizeof (S::i[0]); //  ERROR - non-static data member
}

