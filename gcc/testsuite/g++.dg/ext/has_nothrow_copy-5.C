// PR c++/36872
// { dg-do run }
#include <cassert>

struct S {
    S (const S&) throw ();
    S (int) throw (int);
};

int main ()
{
  assert (__has_nothrow_copy (S));
}
