// PR c++/94719
// { dg-do compile { target concepts } }

template<typename T>
struct bar
{
  template<int N = 5> requires (N == 5)
  bar() { }
};

template<typename T>
struct foo : bar<T>
{
  using foo::bar::bar;
};

void baz()
{
  foo<int>{};
}
