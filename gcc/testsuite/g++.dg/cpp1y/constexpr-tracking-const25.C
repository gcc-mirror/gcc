// PR c++/94772
// { dg-do compile { target c++14 } }

template<int>
struct base
{
  int y{};

  base() = default;

  constexpr base(int a) : base{}
  { y = a; }
};

struct foo : base<1>, base<2>
{
  int x{};

  constexpr foo() : base<2>{}
  {
    ++x; --x;
    ++base<1>::y;
    ++base<2>::y;
  }

  constexpr foo(int a) : base<2>{a}
  {
    x = -base<2>::y;
    ++base<1>::y;
    ++base<2>::y;
  }

  constexpr foo(int a, int b) : foo{a}
  {
    x += a + b;
    ++base<1>::y;
    ++base<2>::y;
  }

  constexpr foo(int a, int b, int c) : base<1>{a}
  {
    x += a + b + c;
    ++base<1>::y;
    ++base<2>::y;
  }
};

#define SA(X) static_assert(X, #X)

int main()
{
  constexpr foo bar1{1, 2};
  SA( bar1.x == 2 );
  SA( bar1.base<1>::y == 2 );
  SA( bar1.base<2>::y == 3 );

  constexpr foo bar2{1, 2, 3};
  SA( bar2.x == 6 );
  SA( bar2.base<1>::y == 2 );
  SA( bar2.base<2>::y == 1 );

  constexpr foo bar3{};
  SA( bar3.x == 0 );
  SA( bar3.base<1>::y == 1 );
  SA( bar3.base<2>::y == 1 );
}
