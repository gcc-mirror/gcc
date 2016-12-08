// { dg-do run { target c++11 } }
#include <cassert>

struct S {
    S (const S&) throw ();
    S (S&&)
#if __cplusplus <= 201402L
    throw (int)			// { dg-warning "deprecated" "" { target { ! c++1z } } }
#endif
    ;
};

int main ()
{
  assert (__has_nothrow_copy (S));
}
