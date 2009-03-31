// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

struct S
{
  int j;
  int i[2]; // { dg-error "" "" { xfail *-*-* } } non-static data member
};

void f ()
{
  sizeof (S::j);
  sizeof (S::i[0]); //  { dg-error "" "" { xfail *-*-* } } used here
}

