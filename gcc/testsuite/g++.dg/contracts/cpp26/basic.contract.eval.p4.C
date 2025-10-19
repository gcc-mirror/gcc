// N5008 :
// basic.contract.eval/p4
// The evaluation of a contract assertion using the ignore semantic has no effect.
// [Note 2 : The predicate is potentially evaluated (6.3), but not evaluated. — end note]
// { dg-do run { target c++26 } }
// { dg-additional-options "-fcontracts -fcontract-evaluation-semantic=ignore" }

#include <contracts>

int f(int i, int j = 1)
  pre (i > 0)
  post (r: r < 5)
{
  contract_assert ( j > 0);
  return i;
}

int main(int, char**)
{
  f (0,0);
}

void
handle_contract_violation (const std::contracts::contract_violation &)
{
  __builtin_abort ();
}
