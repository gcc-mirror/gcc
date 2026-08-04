// { dg-do run { target c++26 } }
// { dg-additional-options "-freflection" }
// No use of Reflection in this test, but it crashed with Reflection
// enabled.

#include <vector>
#include <ranges>

using VVI = std::vector<std::vector<int>>;
using JV = decltype (std::views::join (std::declval<VVI> ()));

static_assert (std::ranges::input_range<JV>);
static_assert (std::ranges::forward_range<JV>);

int
main ()
{
  VVI vv { { 1, 2 }, { 3, 4, 5 } };
  int n = 0;
  for (auto &elt : vv | std::views::join)
    {
      (void) elt;
      ++n;
    }
  return n == 5 ? 0 : 1;
}
