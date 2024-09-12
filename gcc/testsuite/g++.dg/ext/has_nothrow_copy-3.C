// PR c++/36871
// { dg-do run }
// { dg-skip-if "requires hosted libstdc++ for cassert" { ! hostedlib } }
#include <cassert>

struct F {
    F (const F&) throw () { }
    template <class T> F (T) throw () { }
};

int main ()
{
  assert (__has_nothrow_copy (F));
}
