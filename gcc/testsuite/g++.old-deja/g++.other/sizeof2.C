// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

struct S
{
  int j; // { dg-error "" } non-static data member
  int i[2]; // { dg-error "" } non-static data member
};

void f ()
{
  sizeof (S::j); // { dg-error "" } used here
  sizeof (S::i[0]); //  { dg-error "" } used here
}

