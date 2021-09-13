// PR c++/82099
// { dg-do compile { target c++11 } }

template <typename T, typename U>
void bar (T &x, T &y, U u)
{
  u (x, y);
}

template <typename T>
void baz (T &x, T &y) noexcept (noexcept (x == y));

void
foo (int x, int y)
{
  bar (x, y, baz<int>);
}
