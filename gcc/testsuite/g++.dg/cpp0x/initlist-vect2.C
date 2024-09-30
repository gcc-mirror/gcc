// PR c++/108195
// { dg-do run { target c++11 } }
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }

#include <vector>

struct S
{
  S(bool) {}
};

int main()
{
  std::vector<S> v = { true, false, true };
  if (v.size() != 3)
    __builtin_abort ();
}
