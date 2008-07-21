// { dg-do "run" }
// { dg-options "-std=c++0x" }
#include <cassert>

struct S {
    S (const S&) throw ();
    S (S&&) throw (int);
};

int main ()
{
  assert (__has_nothrow_copy (S));
}
