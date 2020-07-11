// PR c++/94772
// { dg-do compile { target c++14 } }

struct foo
{
  int x{};

  constexpr foo() noexcept = default;

  constexpr foo(int a) : foo{}
  { x = -a; }

  constexpr foo(int a, int b) : foo{a}
  { x += a + b; }
};

int main()
{
  constexpr foo bar{1, 2};
  static_assert(bar.x == 2, "");
}
