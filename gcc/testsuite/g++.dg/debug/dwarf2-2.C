// PR debug/27057
// { dg-do compile }
// { dg-options "-g -feliminate-dwarf2-dups" }

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
