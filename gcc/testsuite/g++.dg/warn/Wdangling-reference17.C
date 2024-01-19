// PR c++/111410
// { dg-do compile { target c++20 } }
// { dg-options "-Wdangling-reference" }

#include <vector>
#include <ranges>

int main()
{
  std::vector v{1, 2, 3, 4, 5};
  for (auto i : std::span{v} | std::views::take(1))
    {
      (void) i;
    }
}
