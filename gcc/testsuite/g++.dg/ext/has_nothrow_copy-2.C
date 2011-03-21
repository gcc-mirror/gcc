// PR c++/36871
// { dg-do run }
#include <cassert>

struct A { template <class T> A (T) throw (int); };
struct B { B (B&) throw (); template <class T> B (T) throw (int); };

int main ()
{
  assert (__has_nothrow_copy (A));
  assert (__has_nothrow_copy (B));
}
