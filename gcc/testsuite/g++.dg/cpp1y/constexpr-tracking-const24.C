// PR c++/94772
// { dg-do compile { target c++14 } }

struct base
{
  base() = default;

  constexpr base(int) : base{} { }
};

struct foo : base
{
  int x{};

  constexpr foo(int a) : base{a}
  { x = -a; }

  constexpr foo(int a, int b) : foo{a}
  { x += a + b; }
};

int main()
{
  constexpr foo bar{1, 2};
  static_assert(bar.x == 2, "");
}
