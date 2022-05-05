// PR tree-optimization/104601
// { dg-do run }
// { dg-options "-std=c++17" }

#include <algorithm>
#include <optional>
#include <vector>

inline std::optional<int>
foo (std::vector<int>::iterator b, std::vector<int>::iterator c,
     std::optional<int> h (int))
{
  std::optional<int> d;
  find_if (b, c, [&](auto e) { d = h(e); return d; });
  return d;
}

std::optional<int>
bar (int)
{
  return 1;
}

int
main ()
{
  std::vector<int> g(10);
  auto b = g.begin ();
  auto c = g.end ();
  auto e = foo (b, c, bar);
  if (!e)
    __builtin_abort ();
}
