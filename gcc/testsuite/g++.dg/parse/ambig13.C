// PR c++/64679
// { dg-do compile { target c++11 } }

struct Bar {
  Bar (int, int, int);
};

template<typename T>
void
g ()
{
  int x = 1;
  Bar v1(T(x), T(x), T{x});
  Bar v2(T(x), T(x), T(1));
}

void
invoke (Bar (*p)) noexcept(noexcept(*p))
{
}

auto
pmf (int (Bar::*p)) -> decltype(p)
{
  return nullptr;
}

void
f ()
{
  g<int>();
}
