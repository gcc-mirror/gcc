// PR c++/69743
// { dg-do compile { target c++11 } }

template <typename D, typename... T>
void f(int, T... d)
{
}

template <typename D, typename... T>
void f(T... d)
{
  f<D>(1, d...);
}

void g(void)
{
  f<long>(1.0);
}
