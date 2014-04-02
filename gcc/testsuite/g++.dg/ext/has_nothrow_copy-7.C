// { dg-do run { target c++11 } }
#include <cassert>

struct S {
    S (const S&) throw ();
    S (S&&) throw (int);
};

int main ()
{
  assert (__has_nothrow_copy (S));
}
