// { dg-do compile { target c++17 } }

template <class...T>
constexpr auto f(T... t)
{
  return (... + *t);
}

const int i = 42, j = 24;
static_assert (f(&i,&j) == i+j);
