// { dg-do run }
// { dg-skip-if "requires hosted libstdc++ for cassert" { ! hostedlib } }
#include <cassert>

struct S {
    S (S&) throw ();
    S (const S&, int)
#if __cplusplus <= 201402L
    throw (int)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++17 } } } }
#endif
    ;
};

int main ()
{
  assert (__has_nothrow_copy (S));
}
