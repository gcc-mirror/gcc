// PR c++/36871
// { dg-do "run" }
#include <cassert>

struct F {
    F (const F&) throw () { }
    template <class T> F (T) throw () { }
};

int main ()
{
  assert (__has_nothrow_copy (F));
}
