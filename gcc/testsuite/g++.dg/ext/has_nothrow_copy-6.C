// { dg-do run }
#include <cassert>

struct S {
    S (S&) throw ();
    S (const S&, int) throw (int);
};

int main ()
{
  assert (__has_nothrow_copy (S));
}
