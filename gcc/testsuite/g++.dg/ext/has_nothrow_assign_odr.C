// PR c++/36870
// { dg-do run }


#include <cassert>

struct S { const S& operator= (const S&); };

bool f ();

int main ()
{
  assert (__has_nothrow_assign (S) == f ());
  return 0;
}

const S& S::operator= (const S&a) { return a; }

bool f () { return __has_nothrow_assign (S); }
