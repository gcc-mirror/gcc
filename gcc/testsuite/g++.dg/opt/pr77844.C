// PR debug/77844
// { dg-do compile }
// { dg-options "-O3 -g" }
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }

#include <vector>

void
foo (std::vector<bool>& v, int i, int j)
{
  for (int k = 0; k < 5; ++k)
    v[5 * i + k] = v[5 * i + k] | v[5 * j + k];
}

void
bar (std::vector<bool>& v, int i, int j)
{
  for (int k = 0; k < 5; ++k)
    v[5 * i + k] = v[5 * i + k] ^ v[5 * j + k];
}

void
baz (std::vector<bool>& v)
{
  foo (v, 4, 1);
  foo (v, 4, 2);
  foo (v, 4, 3);
  foo (v, 4, 4);
  bar (v, 4, 1);
  bar (v, 4, 2);
  bar (v, 4, 3);
  bar (v, 4, 4);
}
