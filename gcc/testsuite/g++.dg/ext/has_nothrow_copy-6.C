// { dg-do run }
#include <cassert>

struct S {
    S (S&) throw ();
    S (const S&, int)
#if __cplusplus <= 201402L
    throw (int)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#endif
    ;
};

int main ()
{
  assert (__has_nothrow_copy (S));
}
