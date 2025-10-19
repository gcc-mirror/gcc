// This case should be diagnosed, but we do not handl deferred contracts properly at the moment.
// For now, diagnose that we at least don't accidentally merge the contracts
// { dg-do run { target c++23 } }
// { dg-additional-options "-fcontracts -g3" }
#include <cassert>
struct contract
{
  int checked = 0;
};

contract a, b;

bool
checkA ()
{
  a.checked++;
  return true;
}

bool
checkB ()
{
  b.checked++;
  return true;
}

void
clear_checks ()
{
  a.checked = b.checked = 0;

}

struct S
{
  friend int f1(S) post (checkB());
  friend int f1(S) pre (checkA()){ return 1;};
};


int main()
{
  S s;

  clear_checks ();
  f1(s);
  assert (a.checked > 0);
  assert (b.checked == 0);
}
