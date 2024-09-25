// PR c++/95226
// { dg-do run { target c++14 } }
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }

#include <vector>

struct T {
  unsigned a;
  float b {8.};
};

int main()
{
  T t = {1};
  std::vector<T> tt = {{1}, {2}};
  if (t.a != 1 || t.b != 8.0f || tt[0].a != 1 || tt[0].b != 8.0f || tt[1].a != 2 || tt[1].b != 8.0f)
    __builtin_abort ();
}
