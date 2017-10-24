// PR c++/79590
// { dg-do compile { target c++14 } }

auto f = [](auto x) noexcept(noexcept(x)) { };

int main()
{
  [](auto x) noexcept(noexcept(x)) { } (0);
  [](auto) noexcept(noexcept(0)) { } (0);
}
