// PR c++/99833
// { dg-do compile { target c++20 } }

#include <tuple>

auto f(auto&& x)
{
  [&](auto...) {
    auto y = std::tuple{ "what's happening here?", x };
    if constexpr (auto [_, z] = y; requires { z; })
      return;
  }();
}

int main()
{
  f(42);
}
