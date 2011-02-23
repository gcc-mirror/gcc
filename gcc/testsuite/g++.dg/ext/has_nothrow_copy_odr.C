// PR c++/36870
// { dg-do run }
#include <cassert>

struct S { S (const S&); };

bool f ();

int main ()
{
  assert (__has_nothrow_copy (S) == f ());
}

S::S (const S&) { }

bool f () { return __has_nothrow_copy (S); }
