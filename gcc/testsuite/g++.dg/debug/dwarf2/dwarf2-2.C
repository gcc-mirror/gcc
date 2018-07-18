// PR debug/27057
// { dg-do compile }
// { dg-options "-gdwarf" }

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
