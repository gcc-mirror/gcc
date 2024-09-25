// PR c++/36870
// { dg-do run }
// { dg-skip-if "requires hosted libstdc++ for cassert" { ! hostedlib } }
#include <cassert>

struct S { S (); };

bool f ();

int main ()
{
  assert (__has_nothrow_constructor (S) == f ());
}

S::S () { }

bool f () { return __has_nothrow_constructor (S); }
