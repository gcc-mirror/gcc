// PR c++/111410
// { dg-do compile { target c++20 } }
// { dg-options "-Wdangling-reference" }

#include <vector>
#include <ranges>
#include <span>

int main()
{
  std::vector v{1, 2, 3, 4, 5};
  for (auto i : std::span{v} | std::views::take(1))
    {
      (void) i;
    }

  // From c++/109642.
  const auto vec = std::vector{ 1, 2, 3 };
  const auto s = std::span<decltype(vec)::value_type const>{vec};

  for ([[maybe_unused]] auto _ : s | std::views::take(2)) { }

  for ([[maybe_unused]] auto _ : vec | std::views::take(2)) { }

  const auto s_view = s | std::views::take(2);
  for ([[maybe_unused]] auto _ : s_view) { }
}
