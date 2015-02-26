// PR debug/27057
// { dg-do compile }
// { dg-options "-gdwarf -feliminate-dwarf2-dups" }

namespace N
{
}

struct A
{
  void foo ();
};

void A::foo ()
{
  using namespace N;
}

/* { dg-bogus "-feliminate-dwarf2-dups is broken for C\\+\\+, ignoring" "broken -feliminate-dwarf2-dups" { xfail *-*-* } 1 } */
